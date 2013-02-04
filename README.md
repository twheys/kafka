goethe
======

Erlang game server hopeful


TODO
====

- Fix chat to use registered chat commands (as functions)
- Change IsAdmin in Session to Role
- Remove references to Principle outside of goethe_auth
- Remove Role(rn. ConnectionState) from handle_inbound module function (modules can retrieve it from the session, special case server to handle ConnectionState)
- Change goethe_auth to use handle_internal instead of handle_inbound, and handle inbound in goethe server
- Add cleanup in existing modules for logouts
- Fix JSON parser to accept multiple JSONs in one binary string
- Build sample client (chat application)
- Build Messages modules
- Build Rest API
- Get Build/Update/Release Script Working
- Create Test server
- Add dependencies to Modules through goethe
