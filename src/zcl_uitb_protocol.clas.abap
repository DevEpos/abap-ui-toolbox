"! <p class="shorttext synchronized" lang="en">Protocol display</p>
CLASS zcl_uitb_protocol DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Get instance of protocol</p>
    CLASS-METHODS get_instance
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_uitb_protocol .
    "! <p class="shorttext synchronized" lang="en">Add information message to protocol</p>
    METHODS add_info
      IMPORTING
        !iv_message     TYPE string
        !iv_line_number TYPE zuitb_line_number OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Add information message from sy to protocol</p>
    METHODS add_info_from_sy
      IMPORTING
        !iv_line_number TYPE zuitb_line_number OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">add exception message to protocol</p>
    METHODS add_exception_message
      IMPORTING
        !ir_exception_msg TYPE REF TO zif_uitb_exception_message
        !iv_line_number   TYPE zuitb_line_number OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Add warning message from sy to protocol</p>
    METHODS add_warning_from_sy
      IMPORTING
        !iv_line_number TYPE zuitb_line_number OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Add error message from sy to protocol</p>
    METHODS add_error_from_sy
      IMPORTING
        !iv_line_number TYPE zuitb_line_number OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Add warning message to protocol</p>
    METHODS add_warning
      IMPORTING
        !iv_message     TYPE string
        !iv_line_number TYPE zuitb_line_number OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Add error message to protocol</p>
    METHODS add_error
      IMPORTING
        !iv_message     TYPE string
        !iv_line_number TYPE zuitb_line_number OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Show the protocol</p>
    METHODS show_protocol
      IMPORTING
        !if_show_fullscreen TYPE abap_bool OPTIONAL
        !if_show_as_dialog  TYPE abap_bool OPTIONAL
        !ir_container       TYPE REF TO cl_gui_container OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Close the protocol</p>
    METHODS close_protocol .
    "! <p class="shorttext synchronized" lang="en">Clear the protocol</p>
    METHODS clear .
    "! <p class="shorttext synchronized" lang="en">Checks if protocol holds messages</p>
    METHODS has_messages
      RETURNING
        VALUE(rf_has_messages) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA sr_instance TYPE REF TO zcl_uitb_protocol .
    DATA:
      mt_messages TYPE STANDARD TABLE OF zuitb_protocol_info .
    DATA mr_alv TYPE REF TO zcl_uitb_protocol_alv .

    "! <p class="shorttext synchronized" lang="en">Get system message</p>
    METHODS get_sy_message
      RETURNING
        VALUE(rv_message) TYPE string .
ENDCLASS.



CLASS zcl_uitb_protocol IMPLEMENTATION.


  METHOD add_error.
    APPEND VALUE #(
       type_icon = icon_led_red
       line      = iv_line_number
       message   = iv_message
    ) TO mt_messages.
  ENDMETHOD.


  METHOD add_error_from_sy.
    DATA(lv_message) = get_sy_message( ).

    IF lv_message IS NOT INITIAL.
      add_error(
        iv_message = lv_message
        iv_line_number = iv_line_number
      ).
    ENDIF.
  ENDMETHOD.


  METHOD add_exception_message.
    add_error( iv_message = ir_exception_msg->get_message( )
               iv_line_number = iv_line_number ).
  ENDMETHOD.


  METHOD add_info.
    APPEND VALUE #(
       type_icon = icon_led_green
       line      = iv_line_number
       message   = iv_message
    ) TO mt_messages.
  ENDMETHOD.


  METHOD add_info_from_sy.
    DATA(lv_message) = get_sy_message( ).

    IF lv_message IS NOT INITIAL.
      add_info(
        iv_message = lv_message
        iv_line_number = iv_line_number
      ).
    ENDIF.
  ENDMETHOD.


  METHOD add_warning.
    APPEND VALUE #(
       type_icon = icon_led_yellow
       line      = iv_line_number
       message   = iv_message
    ) TO mt_messages.
  ENDMETHOD.


  METHOD add_warning_from_sy.
    DATA(lv_message) = get_sy_message( ).

    IF lv_message IS NOT INITIAL.
      add_warning(
        iv_message = lv_message
        iv_line_number = iv_line_number
      ).
    ENDIF.
  ENDMETHOD.


  METHOD clear.

    CLEAR mt_messages.

  ENDMETHOD.


  METHOD close_protocol.
    IF mr_alv IS NOT INITIAL.
      mr_alv->close( ).
      CLEAR mr_alv.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF sr_instance IS INITIAL.
      sr_instance = NEW #( ).
    ENDIF.

    rr_instance = sr_instance.
  ENDMETHOD.


  METHOD get_sy_message.
    MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1
                 sy-msgv2
                 sy-msgv3
                 sy-msgv4 INTO rv_message.
  ENDMETHOD.


  METHOD has_messages.
    rf_has_messages = xsdbool( line_exists( mt_messages[ type_icon = icon_led_red ] ) OR
                               line_exists( mt_messages[ type_icon = icon_led_yellow ] ) ).
  ENDMETHOD.


  METHOD show_protocol.
    CHECK mt_messages IS NOT INITIAL.

    close_protocol( ).

    mr_alv = NEW #( mt_messages ).

    IF if_show_fullscreen = abap_true.
      mr_alv->show( ).
    ELSEIF if_show_as_dialog = abap_true.
      mr_alv->show(
          if_show_as_dialog = abap_true
      ).
    ELSE.
      mr_alv->show_docked( iv_dock_at    = cl_gui_docking_container=>dock_at_bottom
                           iv_dock_ratio = 100 ).
    ENDIF.

    clear( ).
  ENDMETHOD.
ENDCLASS.
