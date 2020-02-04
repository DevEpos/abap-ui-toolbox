"! <p class="shorttext synchronized" lang="en">Filters for ALV Columns</p>
CLASS zcl_uitb_alv_filters DEFINITION
  PUBLIC
  INHERITING FROM zcl_uitb_alv_metadata
  FINAL
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_uitb_alv_grid_adapter
                 zcl_uitb_alv_metadata_util.

  PUBLIC SECTION.
    INTERFACES zif_uitb_enumerable.
    "! <p class="shorttext synchronized" lang="en">Add filter to column</p>
    METHODS add_filter
      IMPORTING
        iv_columnname TYPE lvc_fname
        iv_option     TYPE selopt-option DEFAULT 'EQ'
        iv_sign       TYPE selopt-sign DEFAULT 'I'
        iv_low        TYPE zuitb_generic_range-low OPTIONAL
        iv_high       TYPE zuitb_generic_range-high OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_filter
      RAISING
        zcx_uitb_alv_not_found
        zcx_uitb_alv_existing .
    "! <p class="shorttext synchronized" lang="en">Delete all filters</p>
    METHODS clear .
    METHODS refresh.
    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    METHODS constructor
      IMPORTING
        ir_columns    TYPE REF TO zcl_uitb_alv_columns
        ir_controller TYPE REF TO zif_uitb_alv_metadata_ctrller OPTIONAL .

    "! <p class="shorttext synchronized" lang="en">Get filter(s) of given column</p>
    METHODS get_filter
      IMPORTING
        iv_columnname TYPE lvc_fname
      RETURNING
        VALUE(result) TYPE REF TO zcl_uitb_alv_filter
      RAISING
        zcx_uitb_alv_not_found.
    "! <p class="shorttext synchronized" lang="en">Check if there is any filter defined</p>
    METHODS is_filter_defined
      RETURNING
        VALUE(result) TYPE abap_bool.
    "! <p class="shorttext synchronized" lang="en">Delete filter from column</p>
    METHODS remove_filter
      IMPORTING
        iv_columnname TYPE lvc_fname .
    "! <p class="shorttext synchronized" lang="en">Retrieve filtered row count</p>
    METHODS get_filtered_entry_count
      RETURNING
        VALUE(rv_filtered_entries) TYPE i.
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_columns TYPE REF TO zcl_uitb_alv_columns.
    DATA mt_filters TYPE zif_uitb_alv_types=>tt_alv_filter.

    METHODS raise_filter_exists
      IMPORTING
        iv_method     TYPE any OPTIONAL
        iv_columnname TYPE any OPTIONAL
      RAISING
        zcx_uitb_alv_existing.
    "! <p class="shorttext synchronized" lang="en">Repair filter settings</p>
    METHODS repair .
    METHODS raise_filter_not_found
      IMPORTING
        iv_method     TYPE any OPTIONAL
        iv_columnname TYPE any OPTIONAL
      RAISING
        zcx_uitb_alv_not_found.
ENDCLASS.



CLASS zcl_uitb_alv_filters IMPLEMENTATION.


  METHOD add_filter .

    DATA: ls_filter TYPE zif_uitb_alv_types=>ty_alv_filter.

    CHECK iv_columnname IS NOT INITIAL.

    TRY.
        DATA(lr_column) = mr_columns->get_column( iv_columnname ).
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    IF line_exists( mt_filters[ columnname = iv_columnname ] ).
      raise_filter_exists(
        iv_method     = 'ADD_FILTER'
        iv_columnname = iv_columnname ).
    ENDIF.

    ls_filter-columnname = iv_columnname.
    ls_filter-filter_ref = NEW zcl_uitb_alv_filter(
        ir_column     = lr_column
        ir_controller = mr_controller
    ).

    mt_filters = VALUE #( BASE mt_filters ( ls_filter ) ).

    IF ( iv_option IS NOT INITIAL AND
         iv_sign   IS NOT INITIAL AND iv_low IS SUPPLIED ).

      ls_filter-filter_ref->add_selopt(
        iv_sign   = iv_sign
        iv_option = iv_option
        iv_low    = iv_low
        iv_high   = iv_high ).
    ENDIF.

    result = ls_filter-filter_ref.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
      iv_name         = mv_name
      ir_ref          = me
      iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
      iv_refresh_mode = zif_uitb_c_alv_refresh=>full
      iv_method       = 'ADD' ).

  ENDMETHOD.


  METHOD clear.

    CLEAR mt_filters.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
      iv_name         = mv_name
      ir_ref          = me
      iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
      iv_refresh_mode = zif_uitb_c_alv_refresh=>full
      iv_method       = 'CLEAR' ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor(
      io_controller = ir_controller
      iv_name       = 'FILTERS' ).

    mr_columns = ir_columns.

  ENDMETHOD.


  METHOD get_filter .

    ASSIGN mt_filters[ columnname = iv_columnname ] TO FIELD-SYMBOL(<ls_filter>).
    IF sy-subrc = 0.
      result = <ls_filter>-filter_ref.
    ELSE.
      raise_filter_not_found(
        iv_method     = 'GET_FILTER'
        iv_columnname = iv_columnname ).
    ENDIF.

  ENDMETHOD.


  METHOD is_filter_defined.
    result = xsdbool( mt_filters IS NOT INITIAL ).
  ENDMETHOD.


  METHOD raise_filter_exists .

    DATA(lv_class) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->absolute_name.

    RAISE EXCEPTION TYPE zcx_uitb_alv_existing
      EXPORTING
        msgv1 = 'Filter'
        msgv2 = |{ iv_columnname }|
        msgv3 = |{ lv_class }|
        msgv4 = |{ iv_method }|.

  ENDMETHOD.


  METHOD raise_filter_not_found .

    DATA(lv_class) = CAST cl_abap_classdescr( cl_abap_typedescr=>describe_by_object_ref( me ) )->absolute_name.

    RAISE EXCEPTION TYPE zcx_uitb_alv_not_found
      EXPORTING
        msgv1 = 'Filter'
        msgv2 = |{ iv_columnname }|
        msgv3 = |{ lv_class }|
        msgv4 = |{ iv_method }|.

  ENDMETHOD.


  METHOD remove_filter .

    DELETE mt_filters WHERE columnname = iv_columnname.

    CHECK mr_controller IS BOUND.

    mr_controller->set_changed(
      iv_name         = mv_name
      ir_ref          = me
      iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
      iv_refresh_mode = zif_uitb_c_alv_refresh=>full
      iv_method       = 'REMOVE' ).

  ENDMETHOD.


  METHOD repair.

    " repair filters
    DATA(lt_filter) = mt_filters.

    CLEAR mt_filters.

    LOOP AT lt_filter ASSIGNING FIELD-SYMBOL(<ls_filter>).
      DATA(lt_selopt) = <ls_filter>-filter_ref->get( ).

      LOOP AT lt_selopt ASSIGNING FIELD-SYMBOL(<ls_selopt>).
        TRY.
            add_filter(
              iv_columnname  = <ls_filter>-columnname
              iv_sign        = <ls_selopt>-sign
              iv_option      = <ls_selopt>-option
              iv_low         = <ls_selopt>-low
              iv_high        = <ls_selopt>-high ).
          CATCH zcx_uitb_alv_existing
                zcx_uitb_alv_not_found.
        ENDTRY.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD zif_uitb_enumerable~get_element.
    TRY.
        rr_element = mt_filters[ iv_index ]-filter_ref.
      CATCH cx_sy_itab_line_not_found.
        RAISE EXCEPTION TYPE zcx_uitb_element_not_found
          EXPORTING
            index = iv_index.
    ENDTRY.
  ENDMETHOD.


  METHOD zif_uitb_enumerable~get_enumerator.
    rr_enumerator = zcl_uitb_enumerator=>create( me ).
  ENDMETHOD.


  METHOD zif_uitb_enumerable~size.
    rv_size = lines( mt_filters ).
  ENDMETHOD.

  METHOD refresh.
    mr_controller->set_changed(
      iv_name         = mv_name
      ir_ref          = me
      iv_flavour      = zif_uitb_c_alv_chglist_flavor=>setter
      iv_refresh_mode = zif_uitb_c_alv_refresh=>full
      iv_method       = 'REFRESH' ).
  ENDMETHOD.

  METHOD get_filtered_entry_count.
    TRY.
        DATA(lo_adapter) = CAST zcl_uitb_alv_controller( mr_controller )->mo_adapter.
        CHECK lo_adapter IS BOUND.
        DATA(lo_alv) = lo_adapter->get_grid( ).
        CHECK lo_alv IS BOUND.
        lo_alv->get_filtered_entries( IMPORTING et_filtered_entries = DATA(lt_filtered_entries) ).
        rv_filtered_entries = lines( lt_filtered_entries ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
