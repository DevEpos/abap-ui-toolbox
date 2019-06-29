CLASS zcl_uitb_alv_changelist DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create
      IMPORTING
        it_changelist        TYPE zif_uitb_alv_types=>tt_alv_changelist
      RETURNING
        VALUE(rr_changelist) TYPE REF TO zcl_uitb_alv_changelist.

    METHODS is_refresh_requested
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS has_metadata_changed
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS is_selections_requested
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS is_new_data_requested
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS is_functions_change_requested
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS get_refresh_mode
      RETURNING
        VALUE(result) TYPE i.
    METHODS is_filters_only_change
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS is_layout_only_change
      RETURNING
        VALUE(result) TYPE abap_bool.
    DATA mt_changelist TYPE zif_uitb_alv_types=>tt_alv_changelist.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_uitb_alv_changelist IMPLEMENTATION.
  METHOD create.
    rr_changelist = NEW #( ).
    rr_changelist->mt_changelist = it_changelist.
  ENDMETHOD.

  METHOD is_refresh_requested.
    result = xsdbool( line_exists( mt_changelist[ flavour = zif_uitb_c_alv_chglist_flavor=>refresh ] ) ).
  ENDMETHOD.

  METHOD has_metadata_changed.
    result = xsdbool( line_exists( mt_changelist[ flavour = zif_uitb_c_alv_chglist_flavor=>setter ] ) ).
  ENDMETHOD.


  METHOD is_new_data_requested.
    result = xsdbool( line_exists( mt_changelist[ flavour = zif_uitb_c_alv_chglist_flavor=>data_set ] ) ).
  ENDMETHOD.


  METHOD is_functions_change_requested.
    result = xsdbool( line_exists( mt_changelist[ flavour = zif_uitb_c_alv_chglist_flavor=>functions ] ) ).
  ENDMETHOD.

  METHOD get_refresh_mode.
    IF line_exists( mt_changelist[ refresh_mode = zif_uitb_c_alv_refresh=>full ] ).
      result = zif_uitb_c_alv_refresh=>full.
    ELSEIF line_exists( mt_changelist[ refresh_mode = zif_uitb_c_alv_refresh=>soft ] ).
      result = zif_uitb_c_alv_refresh=>soft.
    ENDIF.
  ENDMETHOD.

  METHOD is_selections_requested.
    result = xsdbool( line_exists( mt_changelist[ flavour = zif_uitb_c_alv_chglist_flavor=>selections ] ) ).
  ENDMETHOD.

  METHOD is_filters_only_change.
    DATA(lt_changelist) = mt_changelist.
    DELETE lt_changelist WHERE name <> zif_uitb_c_alv_metadata_types=>filter
                           AND name <> zif_uitb_c_alv_metadata_types=>filters
                           AND frontend = abap_false.

    result = xsdbool( sy-subrc <> 0 ).

  ENDMETHOD.

  METHOD is_layout_only_change.
    DATA(lt_changelist) = mt_changelist.
    DELETE lt_changelist WHERE name <> zif_uitb_c_alv_metadata_types=>display_settings
                           AND frontend = abap_false.

    result = xsdbool( sy-subrc <> 0 ).
  ENDMETHOD.

ENDCLASS.
