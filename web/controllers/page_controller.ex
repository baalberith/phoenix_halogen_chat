defmodule PhoenixHalogenChat.PageController do
  use PhoenixHalogenChat.Web, :controller

  def index(conn, _params) do
    render conn, "index.html"
  end
end
