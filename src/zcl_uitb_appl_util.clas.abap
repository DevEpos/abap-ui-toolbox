class ZCL_UITB_APPL_UTIL definition
  public
  final
  create public .

public section.

  types:
    tt_input_val TYPE STANDARD TABLE OF sval WITH DEFAULT KEY .

  class-methods TOGGLE
    changing
      !VALUE type ABAP_BOOL .
  class-methods GET_PROGRAM_VARIABLES
    importing
      !IV_REPID type SY-REPID
    returning
      value(RT_VARIABLES) type SCOMPOTAB .
  class-methods BUILD_SELOPT
    importing
      !IV_VALUE type ANY
    returning
      value(RT_SELOPT_TABLE) type ZUITB_GENERIC_RANGE_ITAB .
  class-methods BUILD_SELOPT_TAB_FROM_TABLE
    importing
      !IT_TABLE_DATA type STANDARD TABLE
      !IV_COMPNAME_FOR_LOW type FIELDNAME
    returning
      value(RT_SELOPT) type ZUITB_GENERIC_RANGE_ITAB .
  class-methods SPLIT_STRING_FOR_MESSAGE
    importing
      !IV_STRING type STRING
    exporting
      !EV_MSGV1 type SY-MSGV1
      !EV_MSGV2 type SY-MSGV2
      !EV_MSGV3 type SY-MSGV3
      !EV_MSGV4 type SY-MSGV4 .
  class-methods GET_DOCU_TEXT
    importing
      !IV_CLASS type DOKHL-ID default 'DT'
      !IV_OBJECT type DOKHL-OBJECT
    returning
      value(RV_TEXT) type STRING .
  class-methods GET_CURRENT_DATETIME
    exporting
      !EV_TIME type T
      !EV_DATE type D .
  class-methods PRINT_EXC_MESSAGE
    importing
      !IS_TEXTID type SCX_T100KEY
      !IF_TO_SCREEN type ABAP_BOOL default ABAP_TRUE
      !IR_PREVIOUS type ref to CX_ROOT
      !IV_DISPLAY_TYPE type SYST_MSGTY default 'I'
      !IV_MESSAGE_TYPE type SYST_MSGTY default 'E'
      !IR_EXC_MESSAGE type ref to ZIF_UITB_EXCEPTION_MESSAGE
      !IV_MSGV1 type SY-MSGV1 optional
      !IV_MSGV2 type SY-MSGV2 optional
      !IV_MSGV3 type SY-MSGV3 optional
      !IV_MSGV4 type SY-MSGV4 optional
    returning
      value(RV_MESSAGE) type STRING .
  class-methods CONVERT_STRING_TAB_TO_STRING
    importing
      !IT_LINES type STRING_TABLE
    returning
      value(RV_STRING) type STRING .
  class-methods POPUP_TO_CONFIRM
    importing
      !IV_TITLE type STRING default SPACE
      !IV_QUERY type STRING
      !IF_DISPLAY_CANCEL_BUTTON type BOOLEAN default ABAP_TRUE
      !IV_TEXT_BUTTON1 type any default 'Yes'(001)
      !IV_TEXT_BUTTON2 type any default 'No'(002)
      !IV_QUICKINFO_BUTTON1 type TEXT132 optional
      !IV_QUICKINFO_BUTTON2 type TEXT132 optional
      !IV_ICON_TYPE type ICONNAME
    returning
      value(RV_RESULT) type CHAR1 .
  class-methods POPUP_GET_VALUES
    importing
      !IV_TITLE type STRING optional
    changing
      !CT_FIELDS type TT_INPUT_VAL
    returning
      value(RF_SUCCESS) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UITB_APPL_UTIL IMPLEMENTATION.


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
