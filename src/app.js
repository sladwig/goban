import { get, set } from 'idb-keyval';
import ActionCable from 'actioncable';
import Elm from './elm.js';

var key = 'goban';

var App = Elm.Main.init({
  flags: { moves: [] },
  node: document.getElementById('goban'),
});

App.cable = ActionCable.createConsumer(window.cableUrl);
App.retryStack = [];
App.connected = false;

App.retrying = false;
App.retryLater = () => {
  if (App.retrying) return;

  App.retrying = true;
  setTimeout(() => {
    App.retrying = false;
    App.tryNotify();
  }, 3000);
};

App.tryNotify = () => {
  if (!App.connected) App.retryLater();

  var state = App.retryStack.shift();
  if (!state) return; // nothing to do

  if (App.gameChannel.send(state)) return App.tryNotify(); //next

  // something went wrong
  App.retryStack.unshift(state);
  App.retryLater();
};
App.gameChannel = App.cable.subscriptions.create(
  { channel: 'GameChannel', id: 'ab0dc465-fd83-4ad9-86d1-04da44a90f4c' },
  {
    connected: function () {
      App.connected = true;
      App.tryNotify();
    },
    received: function (state) {
      App.ports.loadGame.send(state);
      updateDB(state);
    },
    disconnected: function () {
      App.connected = false;
    },
  },
);

get(key).then((state) => {
  if (!state) return;

  App.ports.loadGame.send(state);
});
const updateState = (newState) => {
  App.retryStack.push(newState);
  App.tryNotify();
};
const updateDB = (newState) => {
  var now = new Date().toISOString().substring(0, 19);
  set(key, newState);
  set('bb-' + now, newState); // backup just in case...
};
App.ports.setStorage.subscribe(function (state) {
  updateDB(state);

  updateState(state);
});
