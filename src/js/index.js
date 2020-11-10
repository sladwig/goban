import { get, set } from 'idb-keyval';
import ActionCable from 'actioncable';


var key = "goban";

var App = Elm.Main.init({ flags: {moves: []}, node: document.getElementById("goban") });
App.cable = ActionCable.createConsumer(import.meta.env.SNOWPACK_PUBLIC_CABLE_URL)
App.retryStack = []
App.connected = false

App.retrying = false
App.retryLater = () => {
	if (App.retrying) return;

	App.retrying = true;
	setTimeout(() => {
		App.retrying = false;
		App.tryNotify();
	}, 3000);
}

App.tryNotify = () => {
	if (!App.connected) App.retryLater();


	var state = App.retryStack.shift()
	if (!state) return; // nothing to do


    if ( App.gameChannel.send(state) ) return App.tryNotify() //next

    // something went wrong
	App.retryStack.unshift(state)
	App.retryLater();
}
App.gameChannel = App.cable.subscriptions.create({ channel: "GameChannel", id: "ab0dc465-fd83-4ad9-86d1-04da44a90f4c"  }, {
	connected: function() {
		App.connected = true
		App.tryNotify()
	}, 
	received: function(d) {
		App.ports.loadGame.send(JSON.stringify(d));
	},
	disconnected: function() {
		App.connected = false
	}
})

get(key).then((dada) => {
	if (!dada) return

	App.ports.loadGame.send(JSON.stringify(dada));
})
const updateState = (newState) => {
	App.retryStack.push(newState)
	App.tryNotify()
}

App.ports.setStorage.subscribe(function (state) {
    var now = new Date().toISOString().substring(0, 19);
   	set(key, state) 
	set("bb-"+now, state) // backup just in case...

	updateState(state)
});

