import * as monaco from 'monaco-editor';
import {RemoteCursorManager, RemoteSelectionManager, EditorContentManager} from '@convergencelabs/monaco-collab-ext';
import { Terminal } from 'xterm';
import './index.css';
import './xterm.css';
import { initProlog } from './prolog.ts';

const initEditor = () => {
	initProlog();

	const editor = monaco.editor.create(document.getElementById("monaco-editor"), {
		value: '% write here',
		language: 'swi-prolog',
		autoIndent: "full",
		theme: "vs-dark",
		automaticLayout: true
	});

	return editor;
};

const initUsername = () => {
	let result = null;
	while (true) {
		result = prompt("Please enter your name", "Harry Potter");
		if (result !== null) {
			return result;
		}
	}
};

class DispatcherEvent {
	eventName: string;
	callbacks: any[];

	constructor(eventName) {
		this.eventName = eventName;
		this.callbacks = [];
	};

	registerCallback(callback) {
		this.callbacks.push(callback);
	}

	unregisterCallback(callback) {
		const index = this.callbacks.indexOf(callback);
		if (index > -1) {
			this.callbacks.splice(index, 1);
		}
	}

	fire(data) {
		const callbacks = this.callbacks.slice(0);
		callbacks.forEach((callback) => { callback(data); });
	}
}

class Dispatcher {
	events: any;

	constructor() {
		this.events = {};
	}

	dispatch(eventName, data) {
		const event = this.events[eventName];
		if (event) { event.fire(data); }
	}

	on(eventName, callback) {
		let event = this.events[eventName];
		if (!event) {
			event = new DispatcherEvent(eventName);
			this.events[eventName] = event;
		}
		event.registerCallback(callback);
	}

	off(eventName, callback) {
		const event = this.events[eventName];
		if (event && event.callbacks.indexOf(callback) > -1) {
			event.unregisterCallback(callback);
			if (event.callbacks.length === 0) {
				delete this.events[eventName];
			}
		}
	}
}

class Client {
	username: string;
	colour: string;

	constructor(username, colour) {
		this.username = username;
		this.colour = colour;
	}
}

const removeAllChildren = (node) => {
	while (node.firstChild) {
		node.removeChild(node.firstChild);
	}
};

const connect = (connection, address) => {
	const socket = new WebSocket(address);
	const message = {tag: 'ClientHello', contents: {helloName: connection.username}};

	socket.addEventListener('open', function (event) {
		console.log("opened!");
		socket.send(JSON.stringify(message));

		socket.addEventListener('close', function(event) {
			alert("connection lost! will try to reconnect in 1 second!");
			setTimeout(() => connect(connection, address), 1000);
		});
	});

	socket.addEventListener('message', function (event) {
		const data = JSON.parse(event.data);
		console.log('DEBUG: Message from server ', data);

		connection.dispatcher.dispatch(data.tag, data.contents);
	});

	socket.addEventListener('close', function (event) {
		console.log("ohno, close!");
	});

	return socket;
};

class Connection {
	socket: any;
	username : string;
	colour? : string;
	dispatcher: Dispatcher;
	users: Map<string, Client>;

	constructor(address, dispatcher, username) {
		this.dispatcher = dispatcher;
		this.users = new Map();
		this.colour = null;
		this.username = username;

		this.socket = connect(this, address);
	}

	on(eventName, callback) {
		this.dispatcher.on(eventName, callback);
	}

	_redrawUsers() {
		console.log("redrawing!");
		let userContainer : HTMLElement = document.getElementsByClassName('user-container')[0] as HTMLElement;
		removeAllChildren(userContainer);
		console.log(userContainer);
		console.log(this.users);
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
		console.log(userContainer2);
		console.log(this.users);
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
		console.log(JSON.stringify(json));
		this.socket.send(JSON.stringify(json));
	}

	sendInsert(index, text) {
		const message = {tag: 'ClientInsert', contents: {insertIndex: index, insertValue: text}};
		this._sendJSON(message);
	}

	sendRemove(index, len) {
		const message = {tag: 'ClientDelete', contents: {deleteIndex: index, deleteLength: len}};
		this._sendJSON(message);
	}

	sendReplace(index, len, text) {
		const message = {tag: 'ClientReplace', contents: {replaceIndex: index, replaceLength: len, replaceValue: text}};
		this._sendJSON(message);
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
			if (!user) { console.log("uhoh cannot find " + name); }

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
			if (!user) { console.log("uhoh cannot find " + name); }

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
		conn.sendInsert(index, text);
	  },
	  onReplace: (index, length, text) => {
		conn.sendReplace(index, length, text);
	  },
	  onDelete: (index, length) => {
		conn.sendRemove(index, length);
	  },
	  remoteSourceId: `convergence`
	});

	conn.on('BroadcastInsert', (contents) => { contentManager.insert(contents.insertIndex, contents.insertValue); });
	conn.on('BroadcastDelete', (contents) => { contentManager.delete(contents.deleteIndex, contents.deleteLength); });
};

const editor = initEditor();
const username = initUsername();

const dispatcher = new Dispatcher();
const conn = new Connection('ws://localhost:8888/stream', dispatcher, username);
const cursorManager = initCursorManager(editor, conn);
const selectionManager = initSelectionManager(editor, conn);
const contentManager = initSharedData(editor, conn);

conn.on('RespondOlleh', (contents) => { 
	console.log('RespondOlleh' + JSON.stringify(contents));

	conn.colour = contents.ollehColour; 
	conn.username = contents.ollehName;
	conn.addClient(new Client(conn.username, conn.colour));

	conn.sendListUsers();
});

conn.on('BroadcastOlleh', (contents) => {
	console.log("olleh");
	console.log(contents);
	console.log("");

	const name = contents.ollehName;
	const colour = contents.ollehColour;
	const client = new Client(name, colour);
	console.log("Adding client! " + name);
	conn.addClient(client);
});

conn.on('RespondUsers', (contents) => {
	conn.users = new Map();
	console.log(contents);
	const allUsers = contents.users;
	contents.users.forEach((user) => {
		const client = new Client(user.userName, user.userColour);
		conn.addClient(client); // this is super inefficient!
	});
});

conn.on('SendCurrentText', (contents) => {
	console.log("current text update!");
	console.log(contents);
	editor.getModel().setValue(contents);
});

const commandId = editor.addCommand(0, () => { conn.sendRefreshText(); }, '');

monaco.languages.registerCodeLensProvider('swi-prolog', {
	provideCodeLenses: function (model, token) {
		return {
			lenses: [
				{
					range: {
						startLineNumber: 1,
						startColumn: 1,
						endLineNumber: 2,
						endColumn: 1
					},
					id: "Force refresh contents",
					command: {
						id: commandId,
						title: "Force refresh contents"
					}
				}
			],
			dispose: () => {}
		};
	},
	resolveCodeLens: function (model, codeLens, token) {
		return codeLens;
	}

});

const terminalInput : HTMLInputElement = document.getElementsByClassName("my-terminal-input")[0] as HTMLInputElement;
const terminalSubmit : HTMLElement = document.getElementsByClassName("my-terminal-submit")[0] as HTMLElement;
const terminalHistory : HTMLInputElement = document.getElementsByClassName("my-terminal-history")[0] as HTMLInputElement;

// on code submit
terminalSubmit.onclick = (event) => {
	const value = terminalInput.value;

	// we explicitly don't set the value here
	// so that everyone has the same answers :)
	conn.sendTerminal(value);
	return false; // prevents page reload
}

conn.on('BroadcastCompilerOutput', (contents) => {
	console.log("compiler output!");
	console.log(contents);
	terminalHistory.value += contents;
	terminalHistory.scrollTop = terminalHistory.scrollHeight;
});

conn.on('BroadcastTerminal', (contents) => {
	console.log("got terminal!");
	terminalHistory.value += "~> " + contents;
	terminalHistory.scrollTop = terminalHistory.scrollHeight;
});

conn.on('BroadcastBye', (contents) => { conn.removeClient(contents.byeName); });


console.log(conn.username);
console.log(conn.colour);
