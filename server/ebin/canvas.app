{application, canvas,
 [{description, "canvas"},
  {vsn, "0.01"},
  {modules, [
    canvas,
    canvas_app,
    canvas_sup,
    canvas_web,
    canvas_deps
  ]},
  {registered, []},
  {mod, {canvas_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
