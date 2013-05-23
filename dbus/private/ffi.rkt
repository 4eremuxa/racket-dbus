#lang racket/base
;
; Low-level D-Bus FFI Bindings
;


(require racket/contract
         racket/match
         (rename-in ffi/unsafe (-> -->))
         ffi/unsafe/define)

(provide (all-defined-out))


(define-ffi-definer define-scheme #f)
(define-ffi-definer define-dbus (ffi-lib "libdbus-1" '("3")))


(define (_string/utf-8/free free-func)
  (make-ctype _bytes
              string->bytes/utf-8
              (lambda (bstr)
                (let ((str (bytes->string/utf-8 bstr)))
                  (free-func bstr)
                  str))))


(define/contract (dbus-check-result result)
                 (-> boolean? void?)
  (unless result
    (error "libdbus failure, maybe out of memory")))


(define-scheme scheme_socket_to_ports
               (_fun _long
                     _string/utf-8
                     _int
                     (inp : (_ptr o _scheme))
                     (outp : (_ptr o _scheme))
                     --> _void
                     --> (begin
                           (register-finalizer inp close-input-port)
                           (register-finalizer outp close-output-port)
                           (values inp outp))))


(define-cpointer-type _DBusConnection-pointer)
(define-cpointer-type _DBusMessage-pointer)
(define-cpointer-type _DBusWatch-pointer)
(define-cpointer-type _DBusTimeout-pointer)
(define-cpointer-type _DBusPendingCall-pointer)


(define-cstruct _DBusError
  ((name     _string/utf-8)
   (message  _string/utf-8)
   (dummy    _uint)
   (padding  _pointer)))


(define-cstruct _DBusMessageIter
  ((dummy1   _pointer)
   (dummy2   _pointer)
   (dummy3   _uint32)
   (dummy4   _int)
   (dummy5   _int)
   (dummy6   _int)
   (dummy7   _int)
   (dummy8   _int)
   (dummy9   _int)
   (dummy10  _int)
   (dummy11  _int)
   (pad1     _int)
   (pad2     _int)
   (pad3     _pointer)))


(define-cstruct _DBusSignatureIter
  ((dummy1  _pointer)
   (dummy2  _pointer)
   (dummy8  _uint32)
   (dummy12 _int)
   (dummy17 _int)))


(define-dbus dbus_free
             (_fun _pointer --> _void))


(define-dbus dbus_error_init
             (_fun _DBusError-pointer --> _void))

(define-dbus dbus_error_free
             (_fun _DBusError-pointer --> _void))

(define-dbus dbus_error_is_set
             (_fun _DBusError-pointer --> _bool))


(define/contract (make-error)
                 (-> DBusError?)
  (let ((error (make-DBusError #f #f 0 #f)))
    (begin
      (dbus_error_init error)
      (register-finalizer error dbus_error_free))
    error))


(define _DBusHandlerResult (_enum '(handled
                                    not-yet-handled
                                    need-memory)))


(define _DBusFreeFunction
  (_fun _pointer --> _void))

(define _DBusAddWatchFunction
  (_fun _DBusWatch-pointer _pointer --> _bool))

(define _DBusRemoveWatchFunction
  (_fun _DBusWatch-pointer _pointer --> _void))

(define _DBusWatchToggledFunction
  (_fun _DBusWatch-pointer _pointer --> _void))

(define _DBusAddTimeoutFunction
  (_fun _DBusTimeout-pointer _pointer --> _bool))

(define _DBusRemoveTimeoutFunction
  (_fun _DBusTimeout-pointer _pointer --> _void))

(define _DBusTimeoutToggledFunction
  (_fun _DBusTimeout-pointer _pointer --> _void))

(define _DBusPendingCallNotifyFunction
  (_fun _DBusPendingCall-pointer _pointer --> _void))

(define _DBusHandleMessageFunction
  (_fun _DBusConnection-pointer
        _DBusMessage-pointer
        _pointer
        --> _DBusHandlerResult))


(define-dbus dbus_threads_init_default
             (_fun --> (result : _bool)
                   --> (dbus-check-result result)))


(define _DBusBusType (_enum '(session system starter)))

(define-dbus dbus_bus_get_private
             (_fun _DBusBusType
                   (error : (_ptr io _DBusError) = (make-error))
                   --> (result : _DBusConnection-pointer)
                   --> (begin
                         (when result
                           (register-finalizer result dbus_connection_unref))
                         (values result error))))

(define-dbus dbus_bus_register
             (_fun _DBusConnection-pointer
                   (error : (_ptr io _DBusError) = (make-error))
                   --> (result : _bool)
                   --> (values result error)))

(define-dbus dbus_bus_add_match
             (_fun _DBusConnection-pointer
                   _string/utf-8
                   (_pointer = #f)
                   --> _void))

(define-dbus dbus_bus_remove_match
             (_fun _DBusConnection-pointer
                   _string/utf-8
                   (_pointer = #f)
                   --> _void))


(define _DBusDispatchStatus (_enum '(data-remains complete need-memory)))

(define-dbus dbus_connection_unref
             (_fun _DBusConnection-pointer --> _void))

(define-dbus dbus_connection_set_watch_functions
             (_fun _DBusConnection-pointer
                   _DBusAddWatchFunction
                   _DBusRemoveWatchFunction
                   _DBusWatchToggledFunction
                   (_pointer = #f)
                   (_DBusFreeFunction = #f)
                   --> (result : _bool)
                   --> (dbus-check-result result)))

(define-dbus dbus_connection_set_timeout_functions
               (_fun _DBusConnection-pointer
                     _DBusAddTimeoutFunction
                     _DBusRemoveTimeoutFunction
                     _DBusTimeoutToggledFunction
                     (_pointer = #f)
                     (_DBusFreeFunction = #f)
                     --> (result : _bool)
                     --> (dbus-check-result result)))

(define-dbus dbus_connection_send
             (_fun _DBusConnection-pointer
                   _DBusMessage-pointer
                   (serial : (_ptr o _uint32))
                   --> (result : _bool)
                   --> (if result serial #f)))

(define-dbus dbus_connection_send_with_reply
             (_fun _DBusConnection-pointer
                   _DBusMessage-pointer
                   (pending-call : (_ptr o _DBusPendingCall-pointer))
                   _int
                   --> (result : _bool)
                   --> (if result
                         (begin
                           (register-finalizer pending-call
                                               dbus_pending_call_unref)
                           pending-call)
                         #f)))

(define-dbus dbus_connection_dispatch
             (_fun _DBusConnection-pointer
                   --> _DBusDispatchStatus))

(define-dbus dbus_connection_get_dispatch_status
             (_fun _DBusConnection-pointer
                   --> _DBusDispatchStatus))


(define _DBusDispatchStatusFunction
  (_fun _DBusConnection-pointer
        _DBusDispatchStatus
        _pointer
        --> _void))


(define-dbus dbus_connection_set_dispatch_status_function
             (_fun _DBusConnection-pointer
                   _DBusDispatchStatusFunction
                   (_pointer = #f)
                   (_DBusFreeFunction = #f)
                   --> _void))

(define-dbus dbus_connection_add_filter
             (_fun _DBusConnection-pointer
                   _DBusHandleMessageFunction
                   (_pointer = #f)
                   (_DBusFreeFunction = #f)
                   --> (result : _bool)
                   --> (dbus-check-result result)))

(define-dbus dbus_connection_remove_filter
             (_fun _DBusConnection-pointer
                   _DBusHandleMessageFunction
                   (_pointer = #f)
                   --> _void))


(define _DBusMessageType (_enum '(invalid call return error signal)))

(define-dbus dbus_message_unref
             (_fun _DBusMessage-pointer --> _void))

(define-dbus dbus_message_new_method_call
             (_fun _string/utf-8
                   _string/utf-8
                   _string/utf-8
                   _string/utf-8
                   --> (result : _DBusMessage-pointer)
                   --> (begin
                         (register-finalizer result dbus_message_unref)
                         result)))

(define-dbus dbus_message_new_signal
             (_fun _string/utf-8
                   _string/utf-8
                   _string/utf-8
                   --> (result : _DBusMessage-pointer)
                   --> (begin
                         (register-finalizer result dbus_message_unref)
                         result)))

(define-dbus dbus_message_set_no_reply
             (_fun _DBusMessage-pointer
                   _bool
                   --> _void))

(define-dbus dbus_message_set_auto_start
             (_fun _DBusMessage-pointer
                   _bool
                   --> _void))

(define-dbus dbus_message_get_type
             (_fun _DBusMessage-pointer
                   --> _DBusMessageType))

(define-dbus dbus_message_is_error
             (_fun _DBusMessage-pointer
                   _string/utf-8
                   --> _bool))

(define-dbus dbus_set_error_from_message
             (_fun (error : (_ptr io _DBusError) = (make-error))
                   _DBusMessage-pointer
                   --> (result : _bool)
                   --> (if result error #f)))

(define-dbus dbus_message_get_path
             (_fun _DBusMessage-pointer
                   --> _string/utf-8))

(define-dbus dbus_message_get_interface
             (_fun _DBusMessage-pointer
                   --> _string/utf-8))

(define-dbus dbus_message_get_member
             (_fun _DBusMessage-pointer
                   --> _string/utf-8))

(define-dbus dbus_message_get_sender
             (_fun _DBusMessage-pointer
                   --> _string/utf-8))


(define _DBusBasicValue (_union _uint8           ;  0
                                _int16           ;  1
                                _uint16          ;  2
                                _int32           ;  3
                                _uint32          ;  4
                                _int64           ;  5
                                _uint64          ;  6
                                _bool            ;  7
                                _double          ;  8
                                _string/utf-8    ;  9
                                _int))           ; 10

(define _DBusType (_enum (list 'invalid      '= 0
                               'byte         '= (char->integer #\y)
                               'boolean      '= (char->integer #\b)
                               'int16        '= (char->integer #\n)
                               'uint16       '= (char->integer #\q)
                               'int32        '= (char->integer #\i)
                               'uint32       '= (char->integer #\u)
                               'int64        '= (char->integer #\x)
                               'uint64       '= (char->integer #\t)
                               'double       '= (char->integer #\d)
                               'string       '= (char->integer #\s)
                               'object-path  '= (char->integer #\o)
                               'signature    '= (char->integer #\g)
                               'unix-fd      '= (char->integer #\h)
                               'array        '= (char->integer #\a)
                               'variant      '= (char->integer #\v)
                               'struct       '= (char->integer #\r)
                               'dict-entry   '= (char->integer #\e))))

(define/contract (make-basic-value type v)
                 (-> symbol? any/c any/c)
  (let ((value (ptr-ref (malloc _DBusBasicValue) _DBusBasicValue)))
    (match type
      ('byte        (union-set! value 0 v))
      ('int16       (union-set! value 1 v))
      ('uint16      (union-set! value 2 v))
      ('int32       (union-set! value 3 v))
      ('uint32      (union-set! value 4 v))
      ('int64       (union-set! value 5 v))
      ('uint64      (union-set! value 6 v))
      ('boolean     (union-set! value 7 v))
      ('double      (union-set! value 8 v))
      ('string      (union-set! value 9 v))
      ('object-path (union-set! value 9 v))
      ('signature   (union-set! value 9 v))
      ('unix-fd     (union-set! value 10 v))
      (_            (error 'make-basic-value "~a not a simple type" type)))
    value))


(define-dbus dbus_message_iter_append_basic
             (_fun _DBusMessageIter-pointer
                   _DBusType
                   (_ptr i _DBusBasicValue)
                   --> (result : _bool)
                   --> (dbus-check-result result)))

(define-dbus dbus_message_iter_abandon_container
             (_fun _DBusMessageIter-pointer
                   _DBusMessageIter-pointer
                   --> _void))

(define-dbus dbus_message_iter_close_container
             (_fun _DBusMessageIter-pointer
                   _DBusMessageIter-pointer
                   --> (result : _bool)
                   --> (dbus-check-result result)))

(define-dbus dbus_message_iter_get_arg_type
             (_fun _DBusMessageIter-pointer
                   --> _DBusType))

(define-dbus dbus_message_iter_get_basic
             (_fun (iter : _DBusMessageIter-pointer)
                   (value : (_ptr o _DBusBasicValue))
                   --> _void
                   --> (let ((t (dbus_message_iter_get_arg_type iter)))
                         (match t
                           ('byte        (union-ref value 0))
                           ('int16       (union-ref value 1))
                           ('uint16      (union-ref value 2))
                           ('int32       (union-ref value 3))
                           ('uint32      (union-ref value 4))
                           ('int64       (union-ref value 5))
                           ('uint64      (union-ref value 6))
                           ('boolean     (union-ref value 7))
                           ('double      (union-ref value 8))
                           ('string      (union-ref value 9))
                           ('object-path (union-ref value 9))
                           ('signature   (union-ref value 9))
                           ('unix-fd     (union-ref value 10))
                           (_            (error 'get-basic-value
                                                "~a not a simple type" t))))))

(define-dbus dbus_message_iter_get_element_type
             (_fun _DBusMessageIter-pointer
                   --> _DBusType))

(define-dbus dbus_message_iter_has_next
             (_fun _DBusMessageIter-pointer
                   --> _bool))

(define-dbus dbus_message_iter_init
             (_fun _DBusMessage-pointer
                   (iter : (_ptr o _DBusMessageIter))
                   --> (result : _bool)
                   --> (if result iter #f)))

(define-dbus dbus_message_iter_init_append
             (_fun _DBusMessage-pointer
                   (iter : (_ptr o _DBusMessageIter))
                   --> _void
                   --> iter))

(define-dbus dbus_message_iter_next
             (_fun _DBusMessageIter-pointer
                   --> _bool))

(define-dbus dbus_message_iter_open_container
             (_fun _DBusMessageIter-pointer
                   _DBusType
                   _string/utf-8
                   (sub : (_ptr o _DBusMessageIter))
                   --> (result : _bool)
                   --> (begin
                         (dbus-check-result result)
                         sub)))

(define-dbus dbus_message_iter_recurse
             (_fun _DBusMessageIter-pointer
                   (sub : (_ptr o _DBusMessageIter))
                   --> _void
                   --> sub))


(define _DBusWatchFlags (_bitmask '(readable = 1
                                    writable = 4
                                    error    = 8
                                    hangup   = 16)))

(define-dbus dbus_watch_get_socket
             (_fun _DBusWatch-pointer
                   --> _int))

(define-dbus dbus_watch_handle
             (_fun _DBusWatch-pointer
                   _DBusWatchFlags
                   --> (result : _bool)
                   --> (dbus-check-result result)))

(define-dbus dbus_watch_get_flags
             (_fun _DBusWatch-pointer
                   --> _DBusWatchFlags))

(define-dbus dbus_watch_get_enabled
             (_fun _DBusWatch-pointer
                   --> _bool))

(define-dbus dbus_pending_call_unref
             (_fun _DBusPendingCall-pointer
                   --> _void))

(define-dbus dbus_pending_call_set_notify
             (_fun _DBusPendingCall-pointer
                   _DBusPendingCallNotifyFunction
                   _pointer
                   (_DBusFreeFunction = #f)
                   --> (result : _bool)
                   --> (dbus-check-result result)))

(define-dbus dbus_pending_call_steal_reply
             (_fun _DBusPendingCall-pointer
                   --> (result : _DBusMessage-pointer)
                   --> (begin
                         (register-finalizer result dbus_message_unref)
                         result)))


(define-dbus dbus_timeout_get_interval
             (_fun _DBusTimeout-pointer
                   --> _int))

(define-dbus dbus_timeout_get_enabled
             (_fun _DBusTimeout-pointer
                   --> _bool))

(define-dbus dbus_timeout_handle
             (_fun _DBusTimeout-pointer
                   --> (result : _bool)
                   --> (dbus-check-result result)))


(define-dbus dbus_signature_iter_init
             (_fun (iter : (_ptr o _DBusSignatureIter))
                   _string/utf-8
                   --> _void
                   --> iter))

(define-dbus dbus_signature_iter_get_current_type
             (_fun _DBusSignatureIter-pointer
                   --> _DBusType))

(define-dbus dbus_signature_iter_next
             (_fun _DBusSignatureIter-pointer
                   --> _bool))

(define-dbus dbus_signature_iter_recurse
             (_fun _DBusSignatureIter-pointer
                   (sub : (_ptr o _DBusSignatureIter))
                   --> _void
                   --> sub))

(define-dbus dbus_signature_iter_get_signature
             (_fun _DBusSignatureIter-pointer
                   --> (_string/utf-8/free dbus_free)))

(define-dbus dbus_signature_validate
             (_fun _string/utf-8
                   (error : (_ptr io _DBusError) = (make-error))
                   --> (result : _bool)
                   --> (values result error)))

(define-dbus dbus_type_is_basic
             (_fun _DBusType --> _bool))


; vim:set ts=2 sw=2 et:
