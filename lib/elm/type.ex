defmodule Elm.Type do
  @type t ::
          {:var, String.t()}
          | {:lambda, t, t}
          | {:tuple, list(t)}
          | {:type, String.t(), list(t)}
          | {:record, list({String.t(), t}), String.t() | nil}
end
