chat-server
================


Ordinary chat server.


Requirements
================

* Haskell Platform 2014.2.0.0
    * https://www.haskell.org/platform/


How to use
================

git-clone and execute `make build && make server`.

```bash
$ make build && make server
```


## Connect with telnet

Run server first, and then:

```bash
$ telnet localhost 3000
Trying ::1...
telnet: connect to address ::1: Connection refused
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
!init 42005  ## server initialized and sent clientId (42005)
!groups   ## Currently there are no groups waiting users.
/new alice 2 10 60  ## Create new group named "alice", capacity 2, timeout 10sec, playtime 60sec
!event join 42006  ## Server responsed the client joined the groupId = 42006
hello?   ## Chatting ...
Client<42005> : hello?
YAHOOOO  ## Chatting
Client<42005> : YAHOOOO
!event leave   ## Server timeout canceler fired, group removed, and client got kicked.
!groups  ## There are no groups again ..
/quit  ## Close connection
Connection closed by foreign host.
```
