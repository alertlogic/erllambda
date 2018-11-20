{application, erllambda_aws_runtime,
 [
  {description, "test AWS Lambda Runtime API"},
  {vsn, "0.1"},
  {applications,
   [kernel,
    stdlib,
    cowboy
   ]},
  {modules, []},
  {mod, {erllambda_aws_runtime, []}},
  {start_phases, [{http_server, []}]},
  {env, [{port, 0}]}
 ]
}.

%% Local Variables:
%% mode: erlang
%% End:
