CLASS zcl_uitb_appl_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_input_val TYPE STANDARD TABLE OF sval WITH EMPTY KEY .

    CLASS-METHODS toggle
      CHANGING
        !value TYPE abap_bool .
    CLASS-METHODS get_program_variables
      IMPORTING
        !iv_repid           TYPE sy-repid
      RETURNING
        VALUE(rt_variables) TYPE scompotab .
    CLASS-METHODS build_selopt
      IMPORTING
        !iv_value              TYPE any
      RETURNING
        VALUE(rt_selopt_table) TYPE zuitb_generic_range_itab .
    CLASS-METHODS build_selopt_tab_from_table
      IMPORTING
        !it_table_data       TYPE STANDARD TABLE
        !iv_compname_for_low TYPE fieldname
      RETURNING
        VALUE(rt_selopt)     TYPE zuitb_generic_range_itab .
    CLASS-METHODS split_string_for_message
      IMPORTING
        !iv_string TYPE string
      EXPORTING
        !ev_msgv1  TYPE sy-msgv1
        !ev_msgv2  TYPE sy-msgv2
        !ev_msgv3  TYPE sy-msgv3
        !ev_msgv4  TYPE sy-msgv4 .
    CLASS-METHODS get_docu_text
      IMPORTING
        !iv_class      TYPE dokhl-id DEFAULT 'DT'
        !iv_object     TYPE dokhl-object
      RETURNING
        VALUE(rv_text) TYPE string .
    CLASS-METHODS get_current_datetime
      EXPORTING
        !ev_time TYPE t
        !ev_date TYPE d .
    CLASS-METHODS print_exc_message
      IMPORTING
        !is_textid        TYPE scx_t100key
        !if_to_screen     TYPE abap_bool DEFAULT abap_true
        !ir_previous      TYPE REF TO cx_root
        !iv_display_type  TYPE syst_msgty DEFAULT 'I'
        !iv_message_type  TYPE syst_msgty DEFAULT 'E'
        !ir_exc_message   TYPE REF TO zif_uitb_exception_message
        !iv_msgv1         TYPE sy-msgv1 OPTIONAL
        !iv_msgv2         TYPE sy-msgv2 OPTIONAL
        !iv_msgv3         TYPE sy-msgv3 OPTIONAL
        !iv_msgv4         TYPE sy-msgv4 OPTIONAL
      RETURNING
        VALUE(rv_message) TYPE string .
    CLASS-METHODS convert_string_tab_to_string
      IMPORTING
        !it_lines        TYPE string_table
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS popup_to_confirm
      IMPORTING
        !iv_title                 TYPE string DEFAULT space
        !iv_query                 TYPE string
        !if_display_cancel_button TYPE boolean DEFAULT abap_true
        !iv_text_button1          TYPE any DEFAULT 'Yes'(001)
        !iv_text_button2          TYPE any DEFAULT 'No'(002)
        !iv_quickinfo_button1     TYPE text132 OPTIONAL
        !iv_quickinfo_button2     TYPE text132 OPTIONAL
        !iv_icon_type             TYPE iconname
      RETURNING
        VALUE(rv_result)          TYPE char1 .

    "! <p class="shorttext synchronized" lang="en">Popup with multiple input fields</p>
    CLASS-METHODS popup_get_values
      IMPORTING
        !iv_title         TYPE string OPTIONAL
      CHANGING
        !ct_fields        TYPE tt_input_val
      RETURNING
        VALUE(rf_success) TYPE abap_bool .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_uitb_appl_util IMPLEMENTATION.


  METHOD build_selopt.
*&---------------------------------------------------------------------*
*& Author: stockbal     Date: 2017/02/08
*&---------------------------------------------------------------------*
*& Description: Creates selopt table for a single value
*&---------------------------------------------------------------------*
    CHECK iv_value IS NOT INITIAL.

    DATA(lv_option) = COND #( WHEN contains( val = iv_value sub = '*' ) THEN
                                'CP'
                              ELSE
                                'EQ' ).

    rt_selopt_table = VALUE #( ( sign = 'I' option = lv_option low = iv_value ) ).
  ENDMETHOD.


  METHOD build_selopt_tab_from_table.
    LOOP AT it_table_data ASSIGNING FIELD-SYMBOL(<ls_line>).
      ASSIGN COMPONENT iv_compname_for_low OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_low_val>).
      APPEND VALUE zuitb_generic_range(
          sign   = 'I'
          option = 'EQ'
          low    = <lv_low_val>
      ) TO rt_selopt.
    ENDLOOP.
  ENDMETHOD.


  METHOD convert_string_tab_to_string.
    LOOP AT it_lines ASSIGNING FIELD-SYMBOL(<lv_line>).
      IF rv_string IS INITIAL.
        rv_string = <lv_line>.
      ELSE.
        rv_string = |{ rv_string }{ cl_abap_char_utilities=>cr_lf }{ <lv_line> }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_current_datetime.
    DATA lv_timestamp_long TYPE timestampl.

    GET TIME STAMP FIELD lv_timestamp_long.
    CONVERT TIME STAMP lv_timestamp_long TIME ZONE 'UTC' INTO TIME ev_time.
    CONVERT TIME STAMP lv_timestamp_long TIME ZONE 'UTC' INTO DATE ev_date.
  ENDMETHOD.


  METHOD get_docu_text.
    DATA: lt_lines TYPE STANDARD TABLE OF tline.

    CALL FUNCTION 'DOCU_GET'
      EXPORTING
        id                = iv_class
        langu             = sy-langu
        object            = iv_object
*       typ               = 'E'    " Dokutyp
*       version           = 0
*       version_active_or_last = 'L'    " Hoechste od. letzte aktive Version
*       print_param_get   = 'X'
*    IMPORTING
*       dokstate          =     " Status des Dokubausteines
*       doktitle          =
*       head              =     " SAPscript-Informationen zum Dokubaustein
*       doktyp            =
      TABLES
        line              = lt_lines
      EXCEPTIONS
        no_docu_on_screen = 1
        no_docu_self_def  = 2
        no_docu_temp      = 3
        ret_code          = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      " convert lines to string
      LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
        IF rv_text IS INITIAL.
          rv_text = <ls_line>-tdline.
        ELSE.
          rv_text = |{ rv_text }{ cl_abap_char_utilities=>cr_lf }{ <ls_line>-tdline }|.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD get_program_variables.
    DATA: lt_includes TYPE STANDARD TABLE OF d010inc,
          lv_type     TYPE char1.

    FIELD-SYMBOLS: <lv_global_data_var> TYPE any.

    " 1) read global data of function group ZSE16J
    CALL FUNCTION 'RS_PROGRAM_INDEX'
      EXPORTING
        pg_name      = iv_repid
      TABLES
        compo        = rt_variables
        inc          = lt_includes
      EXCEPTIONS
        syntax_error = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ENDIF.

    DELETE rt_variables WHERE name CP '<*' OR
                               type <> 'D'.
  ENDMETHOD.


  METHOD popup_get_values.
    DATA: lv_rcode(1).

    rf_success = abap_true.

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title  = iv_title
        start_column = '12'
        start_row    = '5'
      IMPORTING
        returncode   = lv_rcode
      TABLES
        fields       = ct_fields
      EXCEPTIONS
        OTHERS       = 1.

    IF sy-subrc <> 0 OR lv_rcode = 'A'.
      rf_success = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD popup_to_confirm.
    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = iv_query    " Fragetext im Popup
        text_button_1         = iv_text_button1
        text_button_2         = iv_text_button2
        display_cancel_button = if_display_cancel_button
        popup_type            = iv_icon_type
        iv_quickinfo_button_1 = iv_quickinfo_button1
        iv_quickinfo_button_2 = iv_quickinfo_button2
      IMPORTING
        answer                = rv_result.

  ENDMETHOD.


  METHOD print_exc_message.
    IF is_textid-msgid = 'SY' AND is_textid-msgno = 530.

      " try to print message of previous exception
      IF ir_previous IS BOUND.
        TRY.
            DATA(lr_exception_message) = CAST zif_uitb_exception_message( ir_previous ).
            IF if_to_screen = abap_true.
              rv_message = lr_exception_message->print(
                iv_msg_type     = iv_message_type
                iv_display_type = iv_display_type
                if_to_screen    = if_to_screen
              ).
            ELSE.
              rv_message = lr_exception_message->get_message( ).
            ENDIF.
          CATCH cx_sy_move_cast_error.
            MESSAGE ir_previous->get_text( ) TYPE iv_message_type DISPLAY LIKE iv_display_type.
            RETURN.
        ENDTRY.
      ENDIF.
    ELSE.

      IF if_to_screen = abap_true.
        MESSAGE ID is_textid-msgid
                TYPE   iv_message_type
                NUMBER is_textid-msgno
                WITH   iv_msgv1
                       iv_msgv2
                       iv_msgv3
                       iv_msgv4
                DISPLAY LIKE iv_display_type.
      ELSE.
        MESSAGE ID is_textid-msgid
                TYPE   iv_message_type
                NUMBER is_textid-msgno
                WITH   iv_msgv1
                       iv_msgv2
                       iv_msgv3
                       iv_msgv4
                INTO rv_message.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD split_string_for_message.

    DATA: lv_off TYPE i.
    DATA: lv_string TYPE string.

    lv_string = iv_string.

    ev_msgv1 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv2 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv3 = lv_string.
    SHIFT lv_string LEFT BY 50 PLACES.
    ev_msgv4 = lv_string.

    IF strlen( lv_string ) > 50.
      FIND ALL OCCURRENCES OF REGEX '.\s.' IN SECTION LENGTH 47 OF ev_msgv4 MATCH OFFSET lv_off.
      IF sy-subrc = 0.
        lv_off = lv_off + 1.
        ev_msgv4 = ev_msgv4(lv_off).

        ev_msgv4 = |{ ev_msgv4 }...|.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD toggle.
    IF value = abap_true.
      value = abap_false.
    ELSE.
      value = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
