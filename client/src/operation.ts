export type InsertAction = {
	tag: "insert";
	position: number;
	text: string;
};

export type DeleteAction = {
	tag: "delete";
	position: number;
	len: number;
};

export type Action =
	| InsertAction
 	| DeleteAction;

export const serializeAction = (a: Action): any => {
	switch (a.tag) {
		case "insert": {
			return {tag: "ActionInsert", contents: {insertPosition: a.position, insertText: a.text}};
		}
		case "delete": {
			return {tag: "ActionDelete", contents: {deletePosition: a.position, deleteLen: a.len}};
		}
	}
};

export const deserializeAction = (object: any): Action => {
	const tag = object.tag;
	const contents = object.contents;
	switch (tag) {
		case "ActionInsert": {
			const insertAction : InsertAction = {tag: "insert", position: contents.insertPosition, text: contents.insertText};
			return insertAction;
		}
		case "ActionDelete": {
			const deleteAction : DeleteAction = {tag: "delete", position: contents.deletePosition, len: contents.deleteLen};
			return deleteAction;
		}
	}
};

export type Operation = Action[];

export type ActionPair = [Action[], Action[]];

export type StateSynchronized = {
	state: "synchronized";
};

export type StateWaiting = {
	state: "waiting";
	waitingOp: Operation;
};

export type StateWaitingWithBuffer = {
	state: "waitingWithBuffer";
	waitingOp: Operation;
	bufferedOp: Operation;
};

export type State =
	| StateSynchronized
    | StateWaiting
	| StateWaitingWithBuffer;

export type ApplyLocalResult = { 
	clientState: State;
	shouldSendToServer: boolean;
};

export const applyUserOperation = (state: State, op: Operation): ApplyLocalResult => {
	switch (state.state) {
		case "synchronized": {
			const newState : StateWaiting = {state: "waiting", waitingOp: op}; 
			return {clientState: newState, shouldSendToServer: true};
		}
		case "waiting": {
			const newState : StateWaitingWithBuffer = {state: "waitingWithBuffer", waitingOp: state.waitingOp, bufferedOp: op};
			return {clientState: newState, shouldSendToServer: false};
		}
		case "waitingWithBuffer": {
			const newBufferedOp : Operation = state.bufferedOp.concat(op);
			const newState : StateWaitingWithBuffer = {state: "waitingWithBuffer", waitingOp: state.waitingOp, bufferedOp: newBufferedOp};
			return {clientState: newState, shouldSendToServer: false};
		}
	}	
};

export type ApplyServerResult = {
	clientState: State;
	needToApplyThisOperation: Operation;
};

export const applyServerOperation = (state: State, op: Operation): ApplyServerResult => {
	switch (state.state) {
		case "synchronized": {
			return {clientState: state, needToApplyThisOperation: op};
		}
		case "waiting": {
			const [newWaitingOp, needToApplyOp] = transformOperation(state.waitingOp, op);
			const newState : StateWaiting = {state: "waiting", waitingOp: newWaitingOp};
			return {clientState: newState, needToApplyThisOperation: needToApplyOp};
		}
		case "waitingWithBuffer": {
			const [newWaitingOp, newOp] = transformOperation(state.waitingOp, op);
			const [newBufferedOp, needToApplyOp] = transformOperation(state.bufferedOp, newOp);
			const newState : StateWaitingWithBuffer = {state: "waitingWithBuffer", waitingOp: newWaitingOp, bufferedOp: newBufferedOp};
			return {clientState: newState, needToApplyThisOperation: needToApplyOp};
		}
	}	
};

export type ServerAckResult = {
	clientState: State;
	sendMeToServer: Operation | null;
};

export const serverAck = (state: State): ServerAckResult | null => {
	switch (state.state) {
		case "synchronized": {
			return null;
		}
		case "waiting": {
			const newState : StateSynchronized = {state: "synchronized"};
			const result : ServerAckResult = {clientState: newState, sendMeToServer: null};
			return result;
		}
		case "waitingWithBuffer": {
			const newState : StateWaiting = {state: "waiting", waitingOp: state.bufferedOp}; // this is not a mistake!
			const result : ServerAckResult = {clientState: newState, sendMeToServer: state.bufferedOp};
			return result;
		}
	}	
	
};

export const transformOperation = (a: Operation, b: Operation): [Operation, Operation] => {
	const maxLength = Math.max(a.length, b.length);
	let leftResult = [];
	let rightResult = [];
	for (let i = 0; i < maxLength; i++) {
		if (a[i] !== undefined && b[i] !== undefined) {
			const [aActions, bActions] = transformAction(a[i], b[i]);
			leftResult = leftResult.concat(aActions); // TODO(jb): Is this order correct?
			rightResult = rightResult.concat(bActions);
		} else if (a[i] !== undefined) {
			leftResult.push(a[i]);
		} else if (b[i] !== undefined) {
			rightResult.push(b[i]);
		}
	}
	return [leftResult, rightResult];
};

export const transformAction = (a: Action, b: Action): ActionPair => {
	// Why can't I just do `switch ([a.tag, b.tag])`? 
	// TypeScript doesn't do flow checking well enough for it
	// in current version :'(
	switch (a.tag) {
		case "insert": {
			switch (b.tag) {
				case "insert": return transformInsertInsert(a, b);
				case "delete": return transformInsertDelete(a, b);
			}
		}
		case "delete": {
			switch (b.tag) {
				case "insert": {
				    const [newB, newA] = transformInsertDelete(b, a);
				    return [newA, newB];
				}
				case "delete": {
					const newA : DeleteAction = transformDelete(a.position, a.len, b.position, b.len);
					const newB : DeleteAction = transformDelete(b.position, b.len, a.position, a.len);
					return [[newA], [newB]];
				}
			}
		}
	}
};

export const transformInsertInsert = (a: InsertAction, b: InsertAction): [InsertAction[], InsertAction[]] => {
	if (a.position <= b.position) {
		const newB : InsertAction = {tag: "insert", position: (b.position + a.text.length), text: b.text};
		return [[a], [newB]];
	} else {
		const newA : InsertAction = {tag: "insert", position: (a.position + b.text.length), text: a.text};
		return [[newA], [b]];
	}
}

export const transformInsertDelete = (a: InsertAction, b: DeleteAction): [InsertAction[], DeleteAction[]] => {
	if (a.position <= b.position) {
		const newB : DeleteAction = {tag: "delete", position: (b.position + a.text.length), len: b.len};
		return [[a], [newB]];
	} else if (a.position > (b.position + b.len)) {
		const newA : InsertAction = {tag: "insert", position: (a.position - b.len), text: a.text};
		return [[newA], [b]];
	} else {
		const newB1 : DeleteAction = {tag: "delete", position: b.position, len: (a.position - b.position)};
		const newB2 : DeleteAction = {tag: "delete", position: (a.position + a.text.length), len: (b.len - (a.position - b.position))};
		return [[a], [newB1, newB2]];
	}
};

export const transformDelete = (i: number, la: number, j: number, lb: number): DeleteAction => {
	if (j > (i + la)) {
		return {
			tag: "delete",
			position: i,
			len: la
		};
	} else if (i > (j + lb)) {
		return {
			tag: "delete",
			position: (i - lb),
			len: la
		};
	} else if (j < i && (i + la) < (j + lb)) {
		return {
			tag: "delete",
			position: i,
			len: 0
		};
	} else if (j < i && (i + la) > (j + lb)) {
		return {
			tag: "delete",
			position: j,
			len: (i + la - (j + lb))
		};
	} else if (j > i && (j + lb) > (i + la)) {
		return {
			tag: "delete",
			position: i,
			len: (j - i)
		};
	} else {
		return {
			tag: "delete",
			position: i,
			len: (la - lb)
		};
	}
}
