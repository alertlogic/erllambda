{application, erllambda_aws_runtime,
 [
  {description, "test AWS BYOL runtime."},
  {vsn, "0.1"},
  {applications,
   [kernel,
    stdlib,
    cowboy
   ]},
  {modules, []},
  {mod, {erllambda_aws_runtime, []}},
  {start_phases, [{http_server, []}]},
  {env, [{port, 9999}]}
 ]
}.

%% Local Variables:
%% mode: erlang
%% End:
