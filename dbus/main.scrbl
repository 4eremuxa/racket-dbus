#lang scribble/manual

@require[(for-label dbus)
         (for-label racket)]

@title{D-Bus}
@author+email["Jan Dvorak" "mordae@anilinux.org"]

Linux D-Bus bindings for Racket using libdbus-1.

@defmodule[dbus]

@section{Messages}

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

@deftogether[(@defproc[(dbus-signature? (v any/c)) boolean?]
              @defproc[(dbus-interface-name? (v any/c)) boolean?]
              @defproc[(dbus-bus-name? (v any/c)) boolean?]
              @defproc[(dbus-well-known-name? (v any/c)) boolean?]
              @defproc[(dbus-unique-name? (v any/c)) boolean?]
              @defproc[(dbus-object-path? (v any/c)) boolean?]
              @defproc[(dbus-member-name? (v any/c)) boolean?])]{
 Determines whether the value is a valid D-Bus string of respective type.
}

@defproc[(dbus-send-message (bus dbus-connection?)
                            (target dbus-bus-name?)
                            (path dbus-object-path?)
                            (iface dbus-interface-name?)
                            (method dbus-member-name?)
                            (sign dbus-signature?)
                            (arg any/c) ...)
         void?]{
 Send message without expecting a reply.
}

@defproc[(dbus-send-signal (bus dbus-connection?)
                           (path dbus-object-path?)
                           (iface dbus-interface-name?)
                           (name dbus-member-name?)
                           (sign dbus-signature?)
                           (arg any/c) ...)
         void?]{
 Send D-Bus signal.
}

@defproc[(dbus-call (bus dbus-connection?)
                    (target dbus-bus-name?)
                    (path dbus-object-path?)
                    (iface dbus-interface-name?)
                    (method dbus-member-name?)
                    (sign dbus-signature?)
                    (arg any/c) ...)
         any]{
 Calls remote procedure and returns whatever values it produces.
}

@defproc[(dbus-subscribe (bus dbus-connection?)
                         (sender dbus-bus-name?)
                         (path dbus-object-path?)
                         (iface dbus-interface-name?)
                         (signal dbus-member-name?)
                         (handler procedure?))
         void?]{
 Call handler on every matching signal.
}

@defproc[(dbus-unsubscribe (bus dbus-connection?)
                           (sender dbus-bus-name?)
                           (path dbus-object-path?)
                           (iface dbus-interface-name?)
                           (signal dbus-member-name?)
                           (handler procedure?))
         void?]{
 No longer call specified handler for matching signal.
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

@defproc[(dbus-variant-value (v dbus-variant?)) any/c]{
 Return the variant value.
}


@include-section["proxy.scrbl"]
@include-section["introspection.scrbl"]

@; vim:set ft=scribble sw=2 ts=2 et:
