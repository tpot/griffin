% -*- erlang -*-

{application, griffin,
 [{description, "Griffin manageability server."},
  {vsn, "0.1"},
  {modules,   [griffin_app, griffin_sup, cimomhandle, repository]},
  {registered, [griffin_sup, cimomhandle, repository]},
  {applications, [kernel, stdlib]},
  {mod, {griffin_app, []}},
  {env, [{listen_host, "127.0.0.1"}]}
]}.
