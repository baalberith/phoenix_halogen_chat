ExUnit.start

Mix.Task.run "ecto.create", ~w(-r PhoenixHalogenChat.Repo --quiet)
Mix.Task.run "ecto.migrate", ~w(-r PhoenixHalogenChat.Repo --quiet)
Ecto.Adapters.SQL.begin_test_transaction(PhoenixHalogenChat.Repo)

