class ZCL_UITB_PROTOCOL definition
  public
  final
  create private .

public section.

  class-methods GET_INSTANCE
    returning
      value(RR_INSTANCE) type ref to ZCL_UITB_PROTOCOL .
  methods ADD_INFO
    importing
      !IV_MESSAGE type STRING
      !IV_LINE_NUMBER type ZUITB_LINE_NUMBER optional .
  methods ADD_INFO_FROM_SY
    importing
      !IV_LINE_NUMBER type ZUITB_LINE_NUMBER optional .
  methods ADD_EXCEPTION_MESSAGE
    importing
      !IR_EXCEPTION_MSG type ref to ZIF_UITB_EXCEPTION_MESSAGE
      !IV_LINE_NUMBER type ZUITB_LINE_NUMBER optional .
  methods ADD_WARNING_FROM_SY
    importing
      !IV_LINE_NUMBER type ZUITB_LINE_NUMBER optional .
  methods ADD_ERROR_FROM_SY
    importing
      !IV_LINE_NUMBER type ZUITB_LINE_NUMBER optional .
  methods ADD_WARNING
    importing
      !IV_MESSAGE type STRING
      !IV_LINE_NUMBER type ZUITB_LINE_NUMBER optional .
  methods ADD_ERROR
    importing
      !IV_MESSAGE type STRING
      !IV_LINE_NUMBER type ZUITB_LINE_NUMBER optional .
  methods SHOW_PROTOCOL
    importing
      !IF_SHOW_FULLSCREEN type ABAP_BOOL optional
      !IF_SHOW_AS_DIALOG type ABAP_BOOL optional
      !IR_CONTAINER type ref to CL_GUI_CONTAINER optional .
  methods CLOSE_PROTOCOL .
  methods CLEAR .
  methods HAS_MESSAGES
    returning
      value(RF_HAS_MESSAGES) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA sr_instance TYPE REF TO zcl_uitb_protocol .
    DATA:
      mt_messages TYPE STANDARD TABLE OF zuitb_protocol_info .
    DATA mr_alv TYPE REF TO zcl_uitb_protocol_alv .

    METHODS get_sy_message
      RETURNING
        VALUE(rv_message) TYPE string .
ENDCLASS.



CLASS ZCL_UITB_PROTOCOL IMPLEMENTATION.


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
                           iv_dock_ratio = 20 ).
    ENDIF.

    clear( ).
  ENDMETHOD.
ENDCLASS.
