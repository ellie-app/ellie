defmodule Data.Json.Decode do
  @type value :: any

  @type error ::
          {:field, String.t(), error}
          | {:index, integer, error}
          | {:one_of, list(error)}
          | {:failure, String.t(), value}

  @type decoder(a) ::
          :boolean
          | :integer
          | :float
          | :string
          | {:null, a}
          | :value
          | {:list, decoder(a)}
          | {:field, String.t(), decoder(a)}
          | {:index, integer, decoder(a)}
          | {:map, (any -> any), list(decoder(a))}
          | {:and_map, decoder((any -> any)), decoder(a)}
          | {:and_then, (any -> decoder(any)), decoder(a)}
          | {:one_of, list(decoder(a))}
          | {:fail, String.t()}
          | {:succeed, a}

  @spec boolean() :: decoder(boolean)
  def boolean(), do: :boolean

  @spec integer() :: decoder(integer)
  def integer(), do: :integer

  @spec float() :: decoder(float)
  def float(), do: :float

  @spec string() :: decoder(String.t())
  def string(), do: :string

  @spec null(a) :: decoder(a) when a: var
  def null(default), do: {:null, default}

  @spec value() :: decoder(value)
  def value(), do: :value

  @spec list(decoder(a)) :: decoder(list(a)) when a: var
  def list(decoder), do: {:list, decoder}

  @spec field(String.t(), decoder(a)) :: decoder(a) when a: var
  def field(path, decoder), do: {:field, path, decoder}

  @spec index(integer, decoder(a)) :: decoder(a) when a: var
  def index(i, decoder), do: {:index, i, decoder}

  @spec map(decoder(a), (a -> b)) :: decoder(b) when a: var, b: var
  def map(d, f), do: {:map, f, d}

  @spec and_map(decoder((a -> b)), decoder(a)) :: decoder(b) when a: var, b: var
  def and_map(fd, d), do: {:and_map, fd, d}

  @spec and_then(decoder(a), (a -> decoder(b))) :: decoder(b) when a: var, b: var
  def and_then(d, f), do: {:and_then, f, d}

  @spec one_of(list(decoder(a))) :: decoder(a) when a: var
  def one_of(ds), do: {:one_of, ds}

  @spec fail(String.t()) :: decoder(any)
  def fail(message), do: {:fail, message}

  @spec succeed(a) :: decoder(a) when a: var
  def succeed(a), do: {:succeed, a}

  @spec decode_string(String.t(), decoder(a)) :: {:ok, a} | {:error, error} when a: var
  def decode_string(input, decoder) do
    case Poison.decode(input) do
      {:ok, stuff} ->
        run_help(decoder, stuff)

      {:error, :invalid, _} ->
        {:error, {:failure, "This is not valid JSON!", input}}

      {:error, {:invalid, message, _}} ->
        {:error, {:failure, "This is not valid JSON! #{message}", input}}
    end
  end

  @spec decode_value(value, decoder(a)) :: {:ok, a} | {:error, error} when a: var
  def decode_value(value, decoder) do
    run_help(decoder, value)
  end

  defp run_help(decoder, value) do
    case {decoder, value} do
      {:boolean, v} when is_boolean(v) ->
        {:ok, v}

      {:boolean, v} ->
        {:error, {:failure, "Expecting a Boolean", v}}

      {:integer, v} when is_integer(v) ->
        {:ok, v}

      {:integer, v} ->
        {:error, {:failure, "Expecting an Integer", v}}

      {:float, v} when is_float(v) ->
        {:ok, v}

      {:float, v} ->
        {:error, {:failure, "Expecing a Float", v}}

      {:string, v} when is_binary(v) ->
        {:ok, v}

      {:string, v} ->
        {:error, {:failure, "Expecting a String", v}}

      {{:null, default}, v} when is_nil(v) ->
        {:ok, default}

      {{:null, _}, v} ->
        {:error, {:failure, "Expecting null", v}}

      {:value, v} ->
        {:ok, v}

      {{:list, inner}, v} when is_list(v) ->
        case run_array_decoder(inner, v) do
          {:ok, list} -> {:ok, Enum.reverse(list)}
          {:error, reason} -> {:error, reason}
        end

      {{:list, _}, v} ->
        {:error, {:failure, "Expecting a list", v}}

      {{:field, field, inner}, v} ->
        if not is_map(v) || not Map.has_key?(v, field) do
          {:error, {:failure, "Expecting an Object with a field named `#{field}`", v}}
        else
          case run_help(inner, Map.get(v, field)) do
            {:ok, output} -> {:ok, output}
            {:error, reason} -> {:error, {:field, field, reason}}
          end
        end

      {{:index, index, inner}, v} ->
        cond do
          not is_list(v) ->
            {:error, {:failure, "Expecting an Array", v}}

          index >= Enum.count(v) ->
            {:error,
             {:failure,
              "Expecting a longer Array. Need index #{index} but only see #{Enum.count(v)} entries",
              v}}

          true ->
            case run_help(inner, Enum.at(v, index)) do
              {:ok, output} -> {:ok, output}
              {:error, reason} -> {:error, {:index, index, reason}}
            end
        end

      {{:map, f, d}, v} ->
        case run_help(d, v) do
          {:ok, out} -> {:ok, f.(out)}
          {:error, reason} -> {:error, reason}
        end

      {{:and_map, fd, d}, v} ->
        case {run_help(fd, v), run_help(d, v)} do
          {{:ok, f}, {:ok, out}} -> {:ok, f.(out)}
          {{:error, reason}, _} -> {:error, reason}
          {_, {:error, reason}} -> {:error, reason}
        end

      {{:and_then, callback, inner}, v} ->
        case run_help(inner, v) do
          {:error, reason} -> {:error, reason}
          {:ok, a} -> run_help(callback.(a), v)
        end

      {{:one_of, decoders}, v} ->
        Enum.reduce_while(decoders, {:error, {:one_of, []}}, fn inner,
                                                                {:error, {:one_of, errors}} ->
          case run_help(inner, v) do
            {:ok, out} -> {:halt, {:ok, out}}
            {:error, reason} -> {:cont, {:error, {:one_of, [reason | errors]}}}
          end
        end)

      {{:fail, message}, v} ->
        {:error, {:failure, message, v}}

      {{:succeed, a}, _} ->
        {:ok, a}
    end
  end

  defp run_array_decoder(decoder, value) do
    value
    |> Enum.reduce_while({:ok, []}, fn item, memo ->
      case memo do
        {:ok, stuff} ->
          case run_help(decoder, item) do
            {:ok, thing} -> {:cont, {:ok, [thing | stuff]}}
            {:error, reason} -> {:halt, {:error, reason}}
          end

        {:error, reason} ->
          {:halt, {:error, reason}}
      end
    end)
  end
end
