export class DispatcherEvent {
	eventName: string;
	callbacks: any[];

	constructor(eventName) {
		this.eventName = eventName;
		this.callbacks = [];
	};

	registerCallback(callback): void {
		this.callbacks.push(callback);
	}

	unregisterCallback(callback): void {
		const index = this.callbacks.indexOf(callback);
		if (index > -1) {
			this.callbacks.splice(index, 1);
		}
	}

	fire(data): void {
		const callbacks = this.callbacks.slice(0);
		callbacks.forEach((callback) => { callback(data); });
	}
}

export class Dispatcher {
	events: any;

	constructor() {
		this.events = {};
	}

	dispatch(eventName: string, data): void {
		const event = this.events[eventName];
		if (event) { event.fire(data); }
	}

	on(eventName: string, callback: any): void {
		let event = this.events[eventName];
		if (!event) {
			event = new DispatcherEvent(eventName);
			this.events[eventName] = event;
		}
		event.registerCallback(callback);
	}

	off(eventName: string, callback: any): void {
		const event = this.events[eventName];
		if (event && event.callbacks.indexOf(callback) > -1) {
			event.unregisterCallback(callback);
			if (event.callbacks.length === 0) {
				delete this.events[eventName];
			}
		}
	}
}


