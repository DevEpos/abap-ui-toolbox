CLASS zcl_uitb_screen_field_manager DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_repid TYPE sy-repid
        iv_dynnr  TYPE sy-dynnr OPTIONAL.
    METHODS read_values
      IMPORTING
        !it_fieldname_selopt TYPE zuitb_generic_range_itab OPTIONAL
        !iv_step_loop_index  TYPE sy-tabix OPTIONAL
      EXPORTING
        et_field_values      TYPE zuitb_dynpread_itab.
    METHODS read_single_value
      IMPORTING
        !iv_fieldname TYPE dynfnam
      EXPORTING
        !ev_value     TYPE any .
    METHODS read_single_step_loop_value
      IMPORTING
        !iv_fieldname       TYPE dynfnam
        !iv_step_loop_index TYPE sy-tabix OPTIONAL
      EXPORTING
        !ev_value           TYPE any .
    METHODS get_value
      IMPORTING
        !iv_fieldname       TYPE dynfnam
        !iv_step_loop_index TYPE sy-tabix OPTIONAL
      EXPORTING
        !ev_value           TYPE any .
    METHODS set_field_value
      IMPORTING
        iv_name             TYPE dynfnam
        iv_step_loop_index  TYPE sy-tabix OPTIONAL
        iv_value            TYPE any
        if_immediate_update TYPE abap_bool OPTIONAL.
    METHODS update_fields.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mt_dynpread TYPE zuitb_dynpread_itab .
    DATA mv_repid TYPE sy-repid .
    DATA mv_dynnr TYPE sy-dynnr.
    DATA mt_fld_to_update TYPE zuitb_dynpread_itab.


    METHODS read_single_value_internal
      IMPORTING
        it_dynpfield            TYPE zuitb_dynpread_itab
        if_determine_loop_index TYPE abap_bool OPTIONAL
      EXPORTING
        ev_value                TYPE any.
    METHODS fill_update_table
      IMPORTING
        is_screen_field TYPE dynpread.
ENDCLASS.



CLASS zcl_uitb_screen_field_manager IMPLEMENTATION.


  METHOD constructor.
    mv_repid = iv_repid.
    IF iv_dynnr IS INITIAL.
      mv_dynnr = sy-dynnr.
    ENDIF.
  ENDMETHOD.


  METHOD get_value.
    TRY.
        ev_value = COND dynfieldvalue( WHEN iv_step_loop_index > 0 THEN
                                         mt_dynpread[ fieldname = iv_fieldname stepl = iv_step_loop_index ]-fieldvalue
                                       ELSE
                                         mt_dynpread[ fieldname = iv_fieldname ]-fieldvalue ).
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_uitb_screen
          EXPORTING
            textid = zcx_uitb_screen=>field_not_yet_read
            msgv1  = |{ iv_fieldname }|.
    ENDTRY.
  ENDMETHOD.


  METHOD read_single_step_loop_value.
    read_single_value_internal(
      EXPORTING
        it_dynpfield = VALUE #( ( fieldname = iv_fieldname
                                  stepl     = iv_step_loop_index ) )
        if_determine_loop_index = abap_true
      IMPORTING
        ev_value     = ev_value
    ).
  ENDMETHOD.


  METHOD read_single_value.
    read_single_value_internal(
      EXPORTING
        it_dynpfield = VALUE #( ( fieldname = iv_fieldname ) )
      IMPORTING
        ev_value     = ev_value
    ).
  ENDMETHOD.


  METHOD read_single_value_internal.
    DATA(lt_dynpread) = it_dynpfield.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = mv_repid
        dynumb               = sy-dynnr
        determine_loop_index = if_determine_loop_index
      TABLES
        dynpfields           = lt_dynpread    " Tabelle zum Lesen der aktuellen Dynprowerte
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_screen
        EXPORTING
          textid = zcx_uitb_screen=>field_read_error
          msgv1  = |{ sy-dynnr }|.
    ELSE.
      " extract read value
      ev_value = lt_dynpread[ 1 ]-fieldvalue.
    ENDIF.
  ENDMETHOD.


  METHOD read_values.
*&---------------------------------------------------------------------*
*& Description: Reads dynpro fields in current dynpro
*&---------------------------------------------------------------------*
    " clear previously read values
    CLEAR: mt_dynpread.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = mv_repid
        dynumb               = mv_dynnr
        request              = 'A'    " Eingabebereitschaft zurückgeben
      TABLES
        dynpfields           = mt_dynpread    " Tabelle zum Lesen der aktuellen Dynprowerte
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_screen
        EXPORTING
          msgv1 = |{ mv_repid }|
          msgv2 = |{ mv_dynnr }|.
    ELSE.
      " keep only those dynpro fields that match the given fieldname list
      IF it_fieldname_selopt IS NOT INITIAL.
        DELETE mt_dynpread WHERE fieldname NOT IN it_fieldname_selopt.
      ENDIF.
      IF iv_step_loop_index > 0.
        DELETE mt_dynpread WHERE stepl <> iv_step_loop_index.
      ENDIF.
    ENDIF.

    IF et_field_values IS SUPPLIED.
      et_field_values = mt_dynpread.
    ENDIF.
  ENDMETHOD.


  METHOD set_field_value.
*&---------------------------------------------------------------------*
*& Description: Updates a single dynpro field
*&---------------------------------------------------------------------*

    IF iv_step_loop_index > 0.
      ASSIGN mt_dynpread[ fieldname = iv_name stepl = iv_step_loop_index ] TO FIELD-SYMBOL(<ls_dynpro_field>).
    ELSE.
      ASSIGN mt_dynpread[ fieldname = iv_name ] TO <ls_dynpro_field>.
    ENDIF.

    IF <ls_dynpro_field> IS ASSIGNED.
      <ls_dynpro_field>-fieldvalue = iv_value.

      " store field for update
      fill_update_table( EXPORTING is_screen_field = <ls_dynpro_field> ).

      IF if_immediate_update = abap_true.
        update_fields( ).
      ENDIF.
    ELSE.
      RAISE EXCEPTION TYPE zcx_uitb_screen
        EXPORTING
          textid = zcx_uitb_screen=>field_not_yet_read
          msgv1  = |{ iv_name }|
          msgv2  = |{ mv_dynnr }|.
    ENDIF.
  ENDMETHOD.


  METHOD update_fields.
    CHECK mt_fld_to_update IS NOT INITIAL.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = mv_repid
        dynumb               = mv_dynnr
      TABLES
        dynpfields           = mt_fld_to_update
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_uitb_screen
        EXPORTING
          textid = zcx_uitb_screen=>field_update_error
          msgv1  = |{ sy-dynnr }|.
    ENDIF.

    CLEAR mt_fld_to_update.
  ENDMETHOD.

  METHOD fill_update_table.
    ASSIGN mt_fld_to_update[ fieldname = is_screen_field-fieldname ] TO FIELD-SYMBOL(<ls_screen_field>).
    IF sy-subrc <> 0.
      mt_fld_to_update = VALUE #( BASE mt_fld_to_update ( is_screen_field ) ).
    ELSE.
      <ls_screen_field>-fieldvalue = is_screen_field-fieldvalue.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
