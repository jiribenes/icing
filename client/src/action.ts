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


