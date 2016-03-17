Live Haskell
============

An integrated editing and debugging environment for Haskell. It supports Stack projects, and enables fast, interactive evaluation by

1. Running whenever you save the file
2. Running IO within a sandbox (allows for idempotent behavior that still works as if you were running the code live)
3. Showing errors inline
4. Displaying type tooltips
5. Tracing execution so that you can examine variable bindings at a particular line, across all function calls

Screenshots
-----------
![Open](screenshots/Open.png)
Initial open page.

![Help](screenshots/Help.png)
Help screen.

![Type tooltip and inline errors](screenshots/Demo.png)
Showing type tooltip and inline errors.

![Tracing](screenshots/Tracing.png)
Tracing the execution of tail recursive factorial.
