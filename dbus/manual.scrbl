#lang scribble/manual

@require["main.rkt"]

@title{D-Bus}
@author{@(author+email "Jan Dvorak" "mordae@anilinux.org")}

Linux D-Bus bindings for Racket using libdbus-1.

@defmodule[dbus]

@defproc[(exn:fail:dbus? (v any/c)) boolean?]{
 Determines whether the value is a D-Bus exception.
}

@defproc[(exn:fail:dbus-name (exn exn:fail:dbus?)) string?]{
 Returns D-Bus exception name.
}

@defproc[(dbus-open (connection (one-of/c 'session 'system 'starter)))
         dbus-connection?]{
 Connect to session, system or D-Bus daemon that started us.
 Beware: if the BUS dies, the application will exit as well.
}

@defproc[(dbus-connection? (v any/c)) boolean?]{
 Determines whether the value is a D-Bus connection.
}

@defproc[(dbus-send (bus dbus-connection?)
                    (target string?)
                    (path string?)
                    (iface string?)
                    (method string?)
                    (sign dbus-signature?)
                    (arg any/c) ...)
         void?]{
 Send message without expecting a reply.
}

@defproc[(dbus-signal (bus dbus-connection?)
                      (path string?)
                      (iface string?)
                      (name string?)
                      (sign? dbus-signature?)
                      (arg any/c) ...)
         void?]{
 Send D-Bus signal.
}

@defproc[(dbus-call (bus dbus-connection?)
                    (target string?)
                    (path string?)
                    (iface string?)
                    (method string?)
                    (sign dbus-signature?)
                    (arg any/c) ...)
         any]{
 Calls remote procedure and returns whatever values it produces.
}

@defproc[(dbus-variant (sign dbus-signature?)
                       (arg any/c) ...)
         dbus-variant?]{
 Create new variant value.

 Because D-Bus uses eight different numeric types and I don't dare
 to guess which should we marshal numbers to, you have to create the
 variant structure yourself.
}

@defproc[(dbus-variant? (v any/c)) boolean?]{
 Determine whether the value is a variant.
}

@defproc[(dbus-variant-type (v dbus-variant?)) dbus-signature?]{
 Return the variant signature.
}

@defmodule[(dbus-variant-value (v dbus-variant?)) any/c]{
 Return the variant value.
}

@; vim:set ft=scribble sw=2 ts=2 et:
