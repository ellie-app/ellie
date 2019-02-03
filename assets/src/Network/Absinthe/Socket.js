import * as AbsintheSocket from "@absinthe/socket";
import { Socket as PhoenixSocket } from "phoenix";

const createPhoenixSocket = (address, params) =>
  new PhoenixSocket(address, { params });

export default {
  start(app) {
    const onOpen = () => {
      app.ports.absintheSocketInbound.send({ tag: "Open" });
    };

    const onAbort = data => {
      app.ports.absintheSocketInbound.send({ tag: "Abort" });
    };

    const onCancel = data => {
      app.ports.absintheSocketInbound.send({ tag: "Cancel" });
    };

    const onError = data => {
      app.ports.absintheSocketInbound.send({ tag: "Error" });
    };

    const onResult = data => {
      app.ports.absintheSocketInbound.send({ tag: "Data", data });
    };

    app.ports.absintheSocketOutbound.subscribe(data => {
      switch (data.tag) {
        case "Initialize": {
          const phoenixSocket = createPhoenixSocket(data.url, {
            token: data.token
          });
          const absintheSocket = AbsintheSocket.create(phoenixSocket);

          phoenixSocket.onOpen(onOpen);

          const notifier = AbsintheSocket.send(absintheSocket, {
            operation: data.doc,
            variables: {}
          });

          AbsintheSocket.observe(absintheSocket, notifier, {
            onAbort,
            onError,
            onCancel,
            onResult
          });

          break;
        }

        default: {
        }
      }
    });
  }
};
