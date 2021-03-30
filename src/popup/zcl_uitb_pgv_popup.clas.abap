"! <p class="shorttext synchronized" lang="en">Popup for data input</p>
CLASS zcl_uitb_pgv_popup DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE
  GLOBAL FRIENDS zcl_uitb_pgv_factory.

  PUBLIC SECTION.
    INTERFACES zif_uitb_pgv_popup.

    "! <p class="shorttext synchronized" lang="en">Creates new popup</p>
    METHODS constructor
      IMPORTING
        iv_title            TYPE string
        if_no_field_check   TYPE abap_bool OPTIONAL
        if_no_fix_val_check TYPE abap_bool OPTIONAL
        iv_validation_mode  TYPE zif_uitb_pgv_popup=>ty_validation_mode OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES tt_spo_fields TYPE STANDARD TABLE OF sval WITH EMPTY KEY.
    TYPES ty_fld_check_range TYPE RANGE OF spo_value.

    CONSTANTS c_cancelled_subrc TYPE c LENGTH 1 VALUE 'A'.

    DATA mv_start_col TYPE i.
    DATA mv_start_row TYPE i.
    DATA mf_field_user_checks TYPE abap_bool.
    DATA mf_no_field_check TYPE abap_bool.
    DATA mf_no_fix_val_check TYPE abap_bool.
    DATA mv_title TYPE string.
    DATA mv_check_mode TYPE zif_uitb_pgv_popup=>ty_validation_mode.
    DATA mt_fields TYPE zif_uitb_pgv_popup=>tt_dialog_field_ext.
    DATA mf_cancelled TYPE abap_bool.

    METHODS call_dlg.
    METHODS call_dlg_with_user_checks.
    METHODS post_dialog_close
      IMPORTING
        iv_rcode  TYPE c
        it_fields TYPE tt_spo_fields.
    METHODS on_user_field_check
      FOR EVENT check_field_values OF zcl_uitb_pgv_exit_events
      IMPORTING
        eo_fields.
ENDCLASS.



CLASS zcl_uitb_pgv_popup IMPLEMENTATION.

  METHOD constructor.
    mv_title = iv_title.
    mf_no_field_check = if_no_field_check.
    mf_no_fix_val_check = if_no_fix_val_check.
    mv_check_mode = iv_validation_mode.
  ENDMETHOD.

  METHOD zif_uitb_pgv_popup~add_field.
    APPEND VALUE #(
      fieldname   = to_upper( iv_name )
      tabname     = to_upper( iv_tab )
      fieldtext   = iv_label
      field_obl   = if_obligatory
      novaluehlp  = if_no_value_help
      validation  = is_validation
      value       = iv_value
    ) TO mt_fields.
    ro_popup = me.
  ENDMETHOD.

  METHOD zif_uitb_pgv_popup~add_fields.

    LOOP AT it_fields INTO DATA(ls_field).
      ls_field-tabname = to_upper( ls_field-tabname ).
      ls_field-fieldname = to_upper( ls_field-fieldname ).
      APPEND ls_field TO mt_fields.
    ENDLOOP.

    ro_popup = me.
  ENDMETHOD.

  METHOD zif_uitb_pgv_popup~cancelled.
    rf_cancelled = mf_cancelled.
  ENDMETHOD.

  METHOD zif_uitb_pgv_popup~get_field_value.
    rv_result = mt_fields[
      fieldname = iv_name
      tabname   = iv_tab ]-value.
  ENDMETHOD.

  METHOD zif_uitb_pgv_popup~get_field_val_by_index.
    rv_result = mt_fields[ iv_index ]-value.
  ENDMETHOD.

  METHOD zif_uitb_pgv_popup~get_first_field_value.
    rv_result = mt_fields[ 1 ]-value.
  ENDMETHOD.

  METHOD zif_uitb_pgv_popup~show.
    CLEAR mf_cancelled.
    mv_start_col = iv_start_col.
    mv_start_row = iv_start_row.

    IF mv_check_mode = zif_uitb_pgv_popup=>c_validation_modes-no_custom_validation.
      call_dlg( ).
    ELSEIF mv_check_mode = zif_uitb_pgv_popup=>c_validation_modes-inline_user_check.
      SET HANDLER on_user_field_check ACTIVATION abap_true.
      call_dlg_with_user_checks( ).
      SET HANDLER on_user_field_check ACTIVATION abap_false.
    ELSEIF mv_check_mode = zif_uitb_pgv_popup=>c_validation_modes-local_user_check.
      call_dlg_with_user_checks( ).
    ENDIF.

    ro_popup = me.
  ENDMETHOD.

  METHOD call_dlg.
    DATA: lv_rcode TYPE c LENGTH 1.

    DATA(lt_fields) = CORRESPONDING tt_spo_fields( mt_fields ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = mv_title
        start_column    = mv_start_col
        start_row       = mv_start_row
      IMPORTING
        returncode      = lv_rcode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1.

    post_dialog_close(
      iv_rcode  = lv_rcode
      it_fields = lt_fields ).

  ENDMETHOD.

  METHOD call_dlg_with_user_checks.
    DATA: lv_rcode TYPE c LENGTH 1.

    DATA(lt_fields) = CORRESPONDING tt_spo_fields( mt_fields ).

    CALL FUNCTION 'POPUP_GET_VALUES_USER_CHECKED'
      EXPORTING
        popup_title     = mv_title
        start_column    = mv_start_col
        start_row       = mv_start_row
        programname     = 'ZUITB_PGV_EXITS'
        formname        = 'EXIT_CHECK_FIELDS'
      IMPORTING
        returncode      = lv_rcode
      TABLES
        fields          = lt_fields
      EXCEPTIONS
        error_in_fields = 1.

    post_dialog_close(
      iv_rcode  = lv_rcode
      it_fields = lt_fields ).
  ENDMETHOD.

  METHOD on_user_field_check.
    DATA: lt_check_range TYPE ty_fld_check_range.

    APPEND INITIAL LINE TO lt_check_range ASSIGNING FIELD-SYMBOL(<ls_check_range>).
    <ls_check_range>-sign = 'I'.

    LOOP AT eo_fields->get_fields( ) ASSIGNING FIELD-SYMBOL(<ls_input_field>).
      ASSIGN mt_fields[
        fieldname = <ls_input_field>-fieldname
        tabname   = <ls_input_field>-tabname ] TO FIELD-SYMBOL(<ls_field_config>).

      CHECK sy-subrc = 0.
      CHECK <ls_field_config>-validation IS NOT INITIAL.
      <ls_check_range>-option = <ls_field_config>-validation-comparator.
      <ls_check_range>-low = <ls_field_config>-validation-value_low.
      <ls_check_range>-high = <ls_field_config>-validation-value_high.

      DATA(lv_value) = condense( <ls_input_field>-value ).

      IF lv_value NOT IN lt_check_range.
        eo_fields->set_validation_error( VALUE #(
          errorfield = <ls_input_field>-fieldname
          errortab   = <ls_input_field>-tabname
          msgid      = '00'
          msgno      = '001'
          msgv1      = 'Invalid field value' ) ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD post_dialog_close.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_exception.
    ENDIF.

    IF iv_rcode <> c_cancelled_subrc.
      LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<ls_changed_field>).
        ASSIGN mt_fields[ sy-tabix ] TO FIELD-SYMBOL(<ls_field>).
        MOVE-CORRESPONDING <ls_changed_field> TO <ls_field>.
      ENDLOOP.
    ELSE.
      mf_cancelled = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
