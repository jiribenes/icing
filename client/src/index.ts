import * as monaco from 'monaco-editor';
import {RemoteCursorManager, RemoteSelectionManager, EditorContentManager} from '@convergencelabs/monaco-collab-ext';
import { Terminal } from 'xterm';
import './index.css';
import './xterm.css';
// import { initProlog } from './prolog.ts';
import { initHaskell, CompilerOutput, prettyCompilerOutput, compilerOutputToModelMarker } from './haskell.ts';
import { InsertAction, DeleteAction, Action, serializeAction, deserializeAction } from './action.ts';
import { Operation, StateSynchronized, StateWaiting, StateWaitingWithBuffer, State, ApplyLocalResult, applyUserOperation, ApplyServerResult, applyServerOperation, ServerAckResult, serverAck } from './operation.ts';
import { Dispatcher, DispatcherEvent } from './dispatcher.ts';

// this is horrifying
var disableCallback = false;

const debug = false;
const addressBase = "localhost:8888";
const addressSecure = false;

const getEditorAddress = (): string => {
	if (addressSecure) {
		return "wss://" + addressBase + "/stream";
	} else {
		return "ws://" + addressBase + "/stream";
	}
};

const getUsersAddress = (): string => {
	if (addressSecure) {
		return "https://" + addressBase + "/users";
	} else {
		return "http://" + addressBase + "/users";
	}
};

const logDebug = (x: any): void => {
	if (debug) {
		console.log(x);
	}
};


const initEditor = async () => {
	const editor = monaco.editor.create(document.getElementById("monaco-editor"), {
		value: '-- write here',
		language: 'hoskell',
		autoIndent: "full",
		theme: "vs-dark",
		automaticLayout: true,
		readOnly: true,
	    minimap: {
			enabled: false
		},
	});

	editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_Z, () => {
		// lol, no Ctrl+Z for you!
	});

	await initHaskell(editor);


	return editor;
};

const makeEditorWritable = (editor) => {
	editor.updateOptions({ readOnly: false });
};

// For some reason, this is the only proper async function in the whole JS code.
const initUsername = async (): Promise<string> => {
	let triedAlready: boolean = false;
	while (true) {
		try {
			const response = await fetch(getUsersAddress());
			if (response.status !== 200) {
				alert("Something went wrong with the server. Oops.");
			}

			const body: string[] = await response.json();
			logDebug(body);

			const promptText: string = triedAlready ? "Please use a *unique* name. This name might be already taken. Enter your name:" : "Please enter your name:";
			triedAlready = true;
			const promptResult: string | null = prompt(promptText, "Harry Potter");
			if (promptResult !== null) {
				if (body.indexOf(promptResult) === -1) {
					logDebug(promptResult);
					return promptResult;
				}
			}
		} catch(e) {
			alert("Could not connect to the server! Sorry :(");

		}
	}
};
class Client {
	username: string;
	colour: string;

	constructor(username, colour) {
		this.username = username;
		this.colour = colour;
	}
}

/** Removes all children of a given DOM node **/
const removeAllChildren = (node) => {
	while (node.firstChild) {
		node.removeChild(node.firstChild);
	}
};

const connect = (connection, address) => {
	const socket = new WebSocket(address);
	const message = {tag: 'ClientHello', contents: {helloName: connection.username}};

	socket.addEventListener('open', function (event) {
		logDebug("opened!");
		socket.send(JSON.stringify(message));

		socket.addEventListener('close', function(event) {
			alert("connection lost! will try to reconnect in 1 second!");
			setTimeout(() => connect(connection, address), 1000);
		});
	});

	socket.addEventListener('message', function (event) {
		const data = JSON.parse(event.data);
		logDebug('DEBUG: Message from server ');
		logDebug(data);

		connection.dispatcher.dispatch(data.tag, data.contents);
	});

	socket.addEventListener('close', function (event) {
		logDebug("ohno, close!");
	});

	return socket;
};

class Connection {
	socket: any;
	username : string;
	colour? : string;
	dispatcher: Dispatcher;
	revision: number;
	clientState: State;
	users: Map<string, Client>;

	constructor(address, dispatcher, username) {
		this.dispatcher = dispatcher;
		this.users = new Map();
		this.colour = null;
		this.username = username;
		this.revision = 1;
		this.clientState = { state: "synchronized" };

		this.socket = connect(this, address);
	}

	on(eventName, callback) {
		this.dispatcher.on(eventName, callback);
	}

	_redrawUsers() {
		logDebug("redrawing!");
		let userContainer : HTMLElement = document.getElementsByClassName('user-container')[0] as HTMLElement;
		removeAllChildren(userContainer);
		logDebug(userContainer);
		logDebug(this.users);
		let unorderedList = document.createElement('ul');
		this.users.forEach((client, username) => {
			const item = document.createElement('li');
			const text = username + (username === this.username ? " [you]" : "");
			item.appendChild(document.createTextNode(text));
			item.style.color = client.colour;
			unorderedList.appendChild(item);
		});
		document.getElementsByClassName('user-container')[0].appendChild(unorderedList);
		const userContainer2 : HTMLElement = document.getElementsByClassName('user-container')[0] as HTMLElement;
		logDebug(userContainer2);
		logDebug(this.users);
	}

	addClient(client: Client): void {
		this.users.set(client.username, client);
		this._redrawUsers();
	}

	removeClient(username: string): void {
		this.users.delete(username);
		this._redrawUsers();
	}

	getClients(): Map<string, Client> {
		return this.users;
	}

	hasClient(username: string): boolean {
		return this.users.has(username);
	}

	_sendJSON(json) {
		logDebug(JSON.stringify(json));
		this.socket.send(JSON.stringify(json));
	}

	insertText(index, text) {
		const action : InsertAction = {tag: "insert", position: index, text: text};
		const result = applyUserOperation(this.clientState, [action]);
		this.clientState = result.clientState;
		if (result.shouldSendToServer) {
			this.sendOpToServer([action]);
			//const message = {tag: 'ClientInsert', contents: {insertIndex: index, insertValue: text}};
			//this._sendJSON(message);
		}
	}

	removeText(index, len) {
		const action : DeleteAction = {tag: "delete", position: index, len: len};
		const result = applyUserOperation(this.clientState, [action]);
		this.clientState = result.clientState;
		if (result.shouldSendToServer) {
			this.sendOpToServer([action]);
			//const message = {tag: 'ClientDelete', contents: {deleteIndex: index, deleteLength: len}};
			//this._sendJSON(message);
		}
	}

	replaceText(index, len, text) {
		const deleteAction : DeleteAction = {tag: "delete", position: index, len: len};
		const insertAction : InsertAction = {tag: "insert", position: index, text: text};
		const actions : Action[] = [deleteAction, insertAction];
		const result = applyUserOperation(this.clientState, actions);
		this.clientState = result.clientState;
		if (result.shouldSendToServer) {
			this.sendOpToServer(actions);
			//const message = {tag: 'ClientReplace', contents: {replaceIndex: index, replaceLength: len, replaceValue: text}};
			//this._sendJSON(message);
		}
	}

	sendOpToServer(operation: Operation): void {
		const serialized = operation.map(serializeAction);
		const message = {tag: 'ClientSendChanges', contents: {changeRevision: this.revision, changeActions: serialized}};
		logDebug("sending to server!");
		logDebug(message);
		this._sendJSON(message);
	}

	// server sent ACK to our changes, yay!
	handleAck(contents): void {
		logDebug("ACK!");
		const revision: number = contents;
		this.revision = revision;

		const result = serverAck(this.clientState);
		if (result !== null) {
			this.clientState = result.clientState;
			if (result.sendMeToServer !== null) {
				this.sendOpToServer(result.sendMeToServer);
			}
		}
	}

	handleOpFromServer(contents): void {
		logDebug("client state");
		logDebug(this.clientState);
		logDebug("handling op from server");
		logDebug(contents);
		const revision = contents.changeRevision;
		const list = contents.changeActions;
		// list of actions
		const operation : Operation = list.map(deserializeAction);
		logDebug(operation);
		const result = applyServerOperation(this.clientState, operation);
		logDebug(result);
		this.clientState = result.clientState;
		this.revision = revision;
		const applyMe = result.needToApplyThisOperation;
		this.dispatcher.dispatch("InternalApplyOperationFromServer", applyMe);
	}

	sendListUsers() {
		const message = {tag: 'ClientListUsers'}
		this._sendJSON(message);
	}

	sendSetCursor(offset) {
		const message = {tag: 'ClientSetCursor', contents: {clientSetCursorOffset: offset}};
		this._sendJSON(message);
	}

	sendSetSelection(start, end) {
		const message = {tag: 'ClientSetSelection', contents: {clientSetSelectionStart: start, clientSetSelectionEnd: end}};
		this._sendJSON(message);
	}

	sendClearSelection(start, end) {
		const message = {tag: 'ClientClearSelection'};
		this._sendJSON(message);
	}

	sendRefreshText() {
		const message = {tag: 'ClientCurrentText'};
		this._sendJSON(message);
	}

	sendTerminal(text) {
		const message = {tag: 'ClientTerminal', contents: text};
		this._sendJSON(message);
	}
}

const initCursorManager = (editor, conn) => {
	const remoteCursorManager = new RemoteCursorManager({
		editor: editor,
		tooltips: true,
		tooltipDuration: 5
	});

	editor.onDidChangeCursorPosition(e => {
		const position = editor.getPosition();
		const offset = editor.getModel().getOffsetAt(position);
		conn.sendSetCursor(offset);
	});

	let cursors = {};

	conn.on('BroadcastSetCursor', (contents) => { 
		const name = contents.setCursorName;
		if (cursors[name]) {
			let cursor = cursors[name];
			clearTimeout(cursor['timeout']);
			if (cursor['hidden']) {
				cursor['actualCursor'].show();
			}
			cursor['actualCursor'].setOffset(contents.setCursorOffset);
			cursor['timeout'] = setTimeout(() => { 
				cursor['actualCursor'].hide();
				cursor['hidden'] = true;
			}, 10 * 1000);
		} else {
			const user = conn.getClients().get(name);
			if (!user) { logDebug("uhoh cannot find " + name); }

			let actualCursor = remoteCursorManager.addCursor(name, user.colour, name);
			actualCursor.setOffset(contents.setCursorOffset);
			let hidden = false;
			cursors[name] = {actualCursor: actualCursor, hidden: hidden};
			const timeout = setTimeout(() => {
				cursors[name]['actualCursor'].hide();
				cursors[name]['hidden'] = true;
			}, 10 * 1000);
			cursors[name]['timeout'] = timeout;
		}
	});

	conn.on('BroadcastBye', (contents) => {
		const name = contents.byeName;
		if (cursors[name]) {
			cursors[name]['actualCursor'].dispose();
			delete cursors[name];
		}
	});
};

const initSelectionManager = (editor, conn) => {
	const remoteSelectionManager = new RemoteSelectionManager({
		editor: editor,
	});

	editor.onDidChangeCursorSelection(e => {
		const selection = editor.getSelection();
		if (!selection.isEmpty()) {
			const start = editor.getModel().getOffsetAt(selection.getStartPosition());
			const end = editor.getModel().getOffsetAt(selection.getEndPosition());
			conn.sendSetSelection(start, end);
		} else {
			conn.sendClearSelection();
		}
	});

	let selections = {};

	conn.on('BroadcastSetSelection', (contents) => { 
		const name = contents.setSelectionName;
		if (selections[name]) {
			let selection = selections[name];
			clearTimeout(selection['timeout']);
			if (selection['hidden']) {
				selection['actualSelection'].show();
			}
			selection['actualSelection'].setOffsets(contents.setSelectionStart, contents.setSelectionEnd);
			selection['timeout'] = setTimeout(() => { 
				selection['actualSelection'].hide();
				selection['hidden'] = true;
			}, 10 * 1000);
		} else {
			const user = conn.getClients().get(name);
			if (!user) { logDebug("uhoh cannot find " + name); }

			let actualSelection = remoteSelectionManager.addSelection(name, user.colour, name);
			actualSelection.setOffsets(contents.setSelectionStart, contents.setSelectionEnd);
			let hidden = false;
			selections[name] = {actualSelection: actualSelection, hidden: hidden};
			const timeout = setTimeout(() => {
				selections[name]['actualSelection'].hide();
				selections[name]['hidden'] = true;
			}, 10 * 1000);
			selections[name]['timeout'] = timeout;
		}
	});

	conn.on('BroadcastClearSelection', (contents) => {
		const name = contents.clearSelectionName;
		if (selections[name]) {
			selections[name]['actualSelection'].dispose();
			delete selections[name];
		}
	});

	conn.on('BroadcastBye', (contents) => {
		const name = contents.byeName;
		if (selections[name]) {
			selections[name]['actualSelection'].dispose();
			delete selections[name];
		}
	});
};

const initSharedData = (editor, conn) => {
	const contentManager = new EditorContentManager({
	  editor: editor,
	  onInsert: (index, text) => {
		if (!disableCallback) {
			conn.insertText(index, text);
		}
	  },
	  onReplace: (index, len, text) => {
		if (!disableCallback) {
			conn.replaceText(index, len, text);
		}
	  },
	  onDelete: (index, len) => {
		if (!disableCallback) {
			conn.removeText(index, len);
		}
	  },
	  remoteSourceId: `convergence`
	});

	conn.on('InternalApplyOperationFromServer', (actions) => {
		logDebug(actions);
		actions.forEach((action: Action) => {
			switch (action.tag) {
				case "insert": {
					const myAction : InsertAction = action as InsertAction;
					logDebug("inserting from server!");
					logDebug(action);
					logDebug(myAction);
					contentManager.insert(myAction.position, myAction.text);
					return;
				}
				case "delete": {
					const myAction : DeleteAction = action as DeleteAction;
					logDebug("deleting from server!");
					logDebug(action);
					logDebug(myAction);
					contentManager.delete(myAction.position, myAction.len);
					return;
				}
			}
		});
	});
};

const terminalHistory : HTMLInputElement = document.getElementsByClassName("my-terminal-history")[0] as HTMLInputElement;
let terminalQueries : string[] = new Array();
let lastCompilerOutputs : CompilerOutput[] = new Array();

const addTerminalQuery = (query: string): void => {
	if (terminalQueries.length >= 5) {
		terminalQueries.shift();
	}	
	terminalQueries.push(query);
};

const redrawTerminal = (): void => {
	terminalHistory.value = "";
	lastCompilerOutputs.forEach((out) => {
		terminalHistory.value += prettyCompilerOutput(out) + "\n";
	});
	terminalQueries.forEach((terminalQuery) => {
		terminalHistory.value += terminalQuery + "\n";
	});
	terminalHistory.scrollTop = terminalHistory.scrollHeight;
};

const main = () => {
	initEditor().then((editor) => {
		// thanks i really hate this
		initUsername().then((username) => {
			const dispatcher = new Dispatcher();
			const conn = new Connection(getEditorAddress(), dispatcher, username);
			const cursorManager = initCursorManager(editor, conn);
			const selectionManager = initSelectionManager(editor, conn);
			const contentManager = initSharedData(editor, conn);

			conn.on('RespondOlleh', (contents) => { 
				logDebug('RespondOlleh' + JSON.stringify(contents));

				conn.revision = contents.ollehRevision;
				disableCallback = true;
				editor.getModel().setValue(contents.ollehCurrentText);
				disableCallback = false;

				conn.colour = contents.ollehMessage.ollehColour; 
				conn.username = contents.ollehMessage.ollehName;

				conn.addClient(new Client(conn.username, conn.colour));

				conn.sendListUsers();

				// we've caught up, let's open the edutor up!
				makeEditorWritable(editor);
			});

			conn.on('BroadcastOlleh', (contents) => {
				logDebug("olleh");
				logDebug(contents);
				logDebug("");

				const name = contents.ollehName;
				const colour = contents.ollehColour;
				const client = new Client(name, colour);
				logDebug("Adding client! " + name);
				conn.addClient(client);
			});

			conn.on('RespondUsers', (contents) => {
				conn.users = new Map();
				logDebug(contents);
				const allUsers = contents.users;
				contents.users.forEach((user) => {
					const client = new Client(user.userName, user.userColour);
					conn.addClient(client); // this is super inefficient!
				});
			});

			const terminalInput : HTMLInputElement = document.getElementsByClassName("my-terminal-input")[0] as HTMLInputElement;
			const terminalSubmit : HTMLElement = document.getElementsByClassName("my-terminal-submit")[0] as HTMLElement;

			// on code submit
			terminalSubmit.onclick = (event) => {
				const value = terminalInput.value;

				// we explicitly don't set the value here
				// so that everyone has the same answers :)
				conn.sendTerminal(value);
				return false; // prevents page reload
			}

			conn.on('BroadcastCompilerOutput', (contents) => {
				logDebug("compiler output!");
				terminalHistory.value = "";
				const markers : monaco.editor.IMarkerData[] = new Array();
				const compilerOutputs = new Array();
				contents.forEach((msg) => {
					const parsedMsg : CompilerOutput = msg as CompilerOutput;
					logDebug(parsedMsg);
					compilerOutputs.push(parsedMsg);

					const maybeMarker = compilerOutputToModelMarker(parsedMsg);
					if (maybeMarker !== null) {
						markers.push(maybeMarker);
					}

				});

				// put the markers into the document
				monaco.editor.setModelMarkers(editor.getModel(), "Hoskell", markers);

				lastCompilerOutputs = compilerOutputs;
				redrawTerminal();
			});

			conn.on('BroadcastCompilerQuery', (contents) => {
				logDebug("got query!");
				const query = "> " + contents['compilerQueryMessage'];
				const response = "" + contents['compilerQueryResponse'];

				addTerminalQuery(query + "\n" + response);
				redrawTerminal();
			});

			conn.on('BroadcastBye', (contents) => { conn.removeClient(contents.byeName); });

			// handle changes
			conn.on('RespondAck', (contents) => { conn.handleAck(contents); });
			conn.on('BroadcastChanges', (contents) => { conn.handleOpFromServer(contents); });


			logDebug(conn.username);
			logDebug(conn.colour);
		});
	});
};

// call the entry point
main();
