CLASS zcl_uitb_alv_sorts DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_alv_metadata
  FINAL
  CREATE PUBLIC
  GLOBAL FRIENDS zcl_uitb_alv_metadata_util.

  PUBLIC SECTION.
    INTERFACES zif_uitb_enumerable.

    TYPES:
      BEGIN OF ty_alv_sort,
        columnname TYPE lvc_fname,
        ref        TYPE REF TO zcl_uitb_alv_sort,
      END OF ty_alv_sort.
    TYPES:
      tt_alv_sorts TYPE STANDARD TABLE OF ty_alv_sort WITH KEY columnname .

    METHODS add_sort
      IMPORTING
        !iv_column_name TYPE lvc_fname
        !iv_position    TYPE i OPTIONAL
        !iv_sequence    TYPE i DEFAULT if_salv_c_sort=>sort_up
        !if_subtotal    TYPE abap_bool DEFAULT if_salv_c_bool_sap=>false
      RETURNING
        VALUE(result)   TYPE REF TO zcl_uitb_alv_sort
      RAISING
        zcx_uitb_alv_existing.
    METHODS clear .
    METHODS constructor
      IMPORTING
        !ir_columns    TYPE REF TO zcl_uitb_alv_columns
        !ir_controller TYPE REF TO zif_uitb_alv_metadata_ctrller OPTIONAL .
    METHODS get
      RETURNING
        VALUE(result) TYPE tt_alv_sorts.
    METHODS get_sort
      IMPORTING
        iv_column_name TYPE lvc_fname
      RETURNING
        VALUE(result)  TYPE REF TO zcl_uitb_alv_sort
      RAISING
        zcx_uitb_alv_not_found.
    METHODS is_sort_defined
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS remove_sort
      IMPORTING
        iv_column_name TYPE lvc_fname.
    METHODS set_position
      IMPORTING
        !iv_column_name TYPE lvc_fname
        !iv_position    TYPE i OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_columns TYPE REF TO zcl_uitb_alv_columns.
    DATA mt_sorts TYPE tt_alv_sorts.

    METHODS get_subtotals
      RETURNING
        VALUE(result) TYPE tt_alv_sorts.

    METHODS raise_sort_exists
      IMPORTING
        iv_method     TYPE any OPTIONAL
        iv_columnname TYPE any OPTIONAL
      RAISING
        zcx_uitb_alv_existing.
    METHODS raise_sort_not_found
      IMPORTING
        iv_method     TYPE any OPTIONAL
        iv_columnname TYPE any OPTIONAL
      RAISING
        zcx_uitb_alv_not_found.
ENDCLASS.



CLASS zcl_uitb_alv_sorts IMPLEMENTATION.


  METHOD add_sort .

    CHECK iv_column_name IS NOT INITIAL.

    TRY.
        DATA(lr_column) = mr_columns->get_column( iv_column_name ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF line_exists( mt_sorts[ columnname = iv_column_name ] ).
      raise_sort_exists(
        iv_method     = 'ADD_SORT'
        iv_columnname = iv_column_name ).
    ENDIF.

    DATA(ls_sort) = VALUE ty_alv_sort( columnname = iv_column_name ).
    ls_sort-ref = NEW zcl_uitb_alv_sort(
        ir_column     = lr_column
        ir_controller = mr_controller
        iv_sequence   = iv_sequence
        if_subtotal   = if_subtotal
    ).

    IF iv_position IS INITIAL OR iv_position > zif_uitb_enumerable~size( ).
*      IF me->subtotal_compressed IS NOT INITIAL.
*        ls_sort-r_sort->subtotal_compressed = true.
*      ENDIF.
*      APPEND ls_sort TO me->t_sort.
      mt_sorts = VALUE #( BASE mt_sorts ( ls_sort ) ).
    ELSE.
*      IF me->subtotal_compressed IS NOT INITIAL.
*        IF position LE me->subtotal_compressed_index.
*          ADD 1 TO me->subtotal_compressed_index.
*        ELSE.
*          ls_sort-r_sort->subtotal_compressed = true.
*        ENDIF.
*      ENDIF.
      INSERT ls_sort INTO mt_sorts INDEX iv_position.
    ENDIF.

    result = ls_sort-ref.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
      iv_name         = mv_name
      ir_ref          = me
      iv_flavour      = if_salv_c_changelist_flavour=>setter
      iv_refresh_mode = if_salv_c_refresh=>full
      iv_method       = 'ADD' ).

  ENDMETHOD.

  METHOD clear.

    CLEAR mt_sorts.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
        iv_name         = mv_name
        iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
        iv_refresh_mode = zif_uitb_c_alv_refresh=>full
        iv_method       = 'CLEAR'
    ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor(
        io_controller = ir_controller
        iv_name       = 'SORTS'
    ).

    mr_columns = ir_columns.

  ENDMETHOD.


  METHOD get.
    result = mt_sorts.
  ENDMETHOD.


  METHOD get_sort .
    TRY.
        result = mt_sorts[ columnname = iv_column_name ]-ref.
      CATCH cx_sy_itab_line_not_found.
        raise_sort_not_found(
            iv_method     = 'GET_SORT'
            iv_columnname = iv_column_name
        ).
    ENDTRY.

  ENDMETHOD.

  METHOD get_subtotals.
    LOOP AT mt_sorts ASSIGNING FIELD-SYMBOL(<ls_sort>).
      IF <ls_sort>-ref->is_subtotalled( ).
        result = VALUE #( BASE result ( <ls_sort> ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD is_sort_defined.
    result = xsdbool( mt_sorts IS NOT INITIAL ).
  ENDMETHOD.


  METHOD remove_sort .

    DELETE mt_sorts WHERE columnname = iv_column_name.

    CHECK mr_controller IS BOUND.

    set_setter_changed( iv_method = 'REMOVE_SORT' ).
  ENDMETHOD.


  METHOD set_position.

    DATA: ls_sort TYPE ty_alv_sort.

    CHECK iv_column_name IS NOT INITIAL.

    ASSIGN mt_sorts[ columnname = iv_column_name ] TO FIELD-SYMBOL(<ls_sort>).
    IF sy-subrc <> 0 OR sy-tabix = iv_position.
      RETURN.
    ENDIF.

    IF sy-tabix <> iv_position.
      DELETE mt_sorts INDEX sy-tabix.
    ENDIF.

    IF iv_position IS INITIAL OR iv_position > zif_uitb_enumerable~size( ).
      mt_sorts = VALUE #( BASE mt_sorts ( ls_sort ) ).
    ELSE.
      INSERT ls_sort INTO mt_sorts INDEX iv_position.
    ENDIF.

    CHECK mr_controller IS BOUND.

    set_setter_changed( iv_method = 'SET_POSITION' ).
  ENDMETHOD.

  METHOD raise_sort_exists .

    DATA(lv_class) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->absolute_name.

    RAISE EXCEPTION TYPE zcx_uitb_alv_existing
      EXPORTING
        msgv1 = 'Sortierung'
        msgv2 = |{ iv_columnname }|
        msgv3 = |{ lv_class }|
        msgv4 = |{ iv_method }|.

  ENDMETHOD.


  METHOD raise_sort_not_found .

    DATA(lv_class) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->absolute_name.

    RAISE EXCEPTION TYPE zcx_uitb_alv_not_found
      EXPORTING
        msgv1 = 'Sortierung'
        msgv2 = |{ iv_columnname }|
        msgv3 = |{ lv_class }|
        msgv4 = |{ iv_method }|.

  ENDMETHOD.

  METHOD zif_uitb_enumerable~size.
    rv_size = lines( mt_sorts ).
  ENDMETHOD.

  METHOD zif_uitb_enumerable~get_enumerator.
    rr_enumerator = zcl_uitb_enumerator=>create( ir_enumerable = me ).
  ENDMETHOD.

  METHOD zif_uitb_enumerable~get_element.
    TRY.
        rr_element = mt_sorts[ iv_index ]-ref.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_uitb_element_not_found
          EXPORTING
            index = iv_index.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
