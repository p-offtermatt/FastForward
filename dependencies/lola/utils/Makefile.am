noinst_PROGRAMS = listener killer

listener_CPPFLAGS = -I $(top_srcdir)/src
killer_CPPFLAGS = -I $(top_srcdir)/src

listener_SOURCES = remote/Listener.cc \
                   InputOutput/Reporter.cc InputOutput/Reporter.h \
                   InputOutput/SimpleString.cc InputOutput/SimpleString.h \
                   InputOutput/Socket.cc InputOutput/Socket.h

killer_SOURCES = remote/Killer.cc \
                 InputOutput/Reporter.cc InputOutput/Reporter.h \
                 InputOutput/SimpleString.cc InputOutput/SimpleString.h \
                 InputOutput/Socket.cc InputOutput/Socket.h

EXTRA_DIST = groundhog
