defmodule Elm.Version do
  defstruct major: 1, minor: 0, patch: 0

  @type t :: %Elm.Version{major: integer, minor: integer, patch: integer}

  @spec create(major :: integer, minor :: integer, patch :: integer) :: t
  def create(major, minor, patch) do
    %Elm.Version{major: major, minor: minor, patch: patch}
  end

  def to_string(%Elm.Version{major: major, minor: minor, patch: patch}) do
    Integer.to_string(major) <> "." <> Integer.to_string(minor) <> "." <> Integer.to_string(patch)
  end

  def from_string(value) do
    with [major_s, minor_s, patch_s] <- String.split(value, "."),
         {major, _} <- Integer.parse(major_s),
         {minor, _} <- Integer.parse(minor_s),
         {patch, _} <- Integer.parse(patch_s) do
      {:ok, %Elm.Version{major: major, minor: minor, patch: patch}}
    else
      _ -> :error
    end
  end
end
