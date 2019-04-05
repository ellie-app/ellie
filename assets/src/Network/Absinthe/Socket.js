export default {
  start: app => {
    const initialize = ({ url, token, doc }) => {
      return Promise.all([
        import(/* webpackChunkName: "phoenix" */ "phoenix"),
        import(/* webpackChunkName: "absinthe-socket" */ "@absinthe/socket")
      ]).then(([Phoenix, AbsintheSocket]) => {
        const phoenixSocket = new Phoenix.Socket(url, { params: { token } });
        const absintheSocket = AbsintheSocket.create(phoenixSocket);

        const notifier = AbsintheSocket.send(absintheSocket, {
          operation: doc,
          variables: {}
        });

        AbsintheSocket.observe(absintheSocket, notifier, {
          onStart,
          onAbort,
          onError,
          onCancel,
          onResult
        });
      });
    };

    const onStart = _data => {
      app.ports.absintheSocketInbound.send({ tag: "Open" });
    };

    const onAbort = _data => {
      app.ports.absintheSocketInbound.send({ tag: "Abort" });
    };

    const onCancel = _data => {
      app.ports.absintheSocketInbound.send({ tag: "Cancel" });
    };

    const onError = _data => {
      app.ports.absintheSocketInbound.send({ tag: "Error" });
    };

    const onResult = data => {
      app.ports.absintheSocketInbound.send({ tag: "Data", data });
    };

    app.ports.absintheSocketOutbound.subscribe(data => {
      switch (data.tag) {
        case "Initialize": {
          initialize(data);
          break;
        }

        default: {
        }
      }
    });
  }
};
