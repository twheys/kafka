{application, goethe,
[{description, "Goethe Game Server"},
    {vsn, "1"},
    {modules, [goethe,goethe_core,goethe_socket,goethe_sup]},
    {registered, []},
    {applications, [kernel,stdlib]},
    {mod, {goethe_app, []}},
    {start_Phases, []}
]}.
