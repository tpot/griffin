% -*- erlang -*-

{application, griffin,
 [{description, "Griffin manageability server."},
  {vsn, "0.1"},
  {modules,   [griffin_app, griffin_sup]},
  {registered, [griffin_sup]},
  {applications, [kernel, stdlib]},
  {mod, {griffin_app, []}}
]}.
