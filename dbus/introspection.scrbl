#lang scribble/manual

@require[(for-label racket)]

@title{Introspection}
@defmodule[dbus/introspection]


@defproc[(dbus-introspect (bus dbus-connection?)
                          (endpoint dbus-bus-name?)
                          (path dbus-object-path?)) dbus-object?]{
 Return proxy for specified object with methods, properties and signals
 populated using D-Bus introspection mechanism.
}

@; vim:set ft=scribble sw=2 ts=2 et:
