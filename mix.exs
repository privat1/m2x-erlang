defmodule M2X.Mixfile do
  use Mix.Project

  def version, do: "1.3.0" # Version number must also be updated in both git m2x.erl and m2x.app.src

  def project do
    [ app:         :m2x_erlang,
      version:     version,
      language:    :erlang,
      deps:        dependencies,
      description: description,
      package:     package ]
  end

  def application do [] end

  defp dependencies do
    [ hackney: "~> 1.0",
      jsx:     "~> 2.5" ]
  end

  defp description do
    """
    Erlang client library for the AT&T M2X (http://m2x.att.com) API.
    AT&T M2X is a cloud-based fully managed time-series data storage service
    for network connected machine-to-machine (M2M) devices and the
    Internet of Things (IoT).
    """
  end

  defp package do
    [ maintainers: ["Joe McIlvain"],
      licenses:    ["MIT"],
      links: %{
        "GitHub"   => "https://github.com/attm2x/m2x-erlang",
        "API Docs" => "https://m2x.att.com/developer/documentation"
      } ]
  end
end
