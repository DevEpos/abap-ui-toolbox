CLASS zcl_uitb_data_cache DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">Get current instance of data cache</p>
    "!
    "! Each program has its own cache, so the program id is necessary to retrieve
    "! the correct instance
    "!
    "! @parameter iv_repid | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rr_instance | <p class="shorttext synchronized" lang="en"></p>
    CLASS-METHODS get_instance
      IMPORTING
        !iv_repid          TYPE sy-repid
      RETURNING
        VALUE(rr_instance) TYPE REF TO zcl_uitb_data_cache .
    "! <p class="shorttext synchronized" lang="en">Check if cache for report is initialized</p>
    "!
    "! @parameter rf_cache_is_filled | <p class="shorttext synchronized" lang="en"></p>
    METHODS is_cache_initialized
      RETURNING
        VALUE(rf_cache_is_filled) TYPE abap_bool .
    "! <p class="shorttext synchronized" lang="en">Registers data reference under given name</p>
    "!
    "! @parameter ir_variable_ref | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter iv_variable_name | <p class="shorttext synchronized" lang="en"></p>
    METHODS register_data
      IMPORTING
        !ir_variable_ref  TYPE any
        !iv_variable_name TYPE string .
    "! <p class="shorttext synchronized" lang="en">Retrieve data reference for given name</p>
    "!
    "! @parameter iv_registered_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter rr_data_ref | <p class="shorttext synchronized" lang="en"></p>
    METHODS get_data_ref
      IMPORTING
        !iv_registered_name TYPE string
      RETURNING
        VALUE(rr_data_ref)  TYPE REF TO data .
    "! <p class="shorttext synchronized" lang="en">Updates value of object reference</p>
    "!
    "! @parameter iv_registered_name | <p class="shorttext synchronized" lang="en"></p>
    "! @parameter ir_new_object_ref | <p class="shorttext synchronized" lang="en"></p>
    METHODS update_object_ref
      IMPORTING
        !iv_registered_name TYPE string
        !ir_new_object_ref  TYPE REF TO object .
    "! <p class="shorttext synchronized" lang="en">Clears object reference</p>
    METHODS clear_object_ref
      IMPORTING
        !iv_registered_name TYPE string .
    "! <p class="shorttext synchronized" lang="en">Clears the object cache</p>
    "!
    METHODS clear_cache .
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF lty_cache_map,
             cache_id  TYPE sy-repid,
             cache     TYPE HASHED TABLE OF zuitb_global_data_ref WITH UNIQUE KEY field_name,
             screen_id TYPE sy-dynnr,
           END OF lty_cache_map.
    TYPES: ltt_cache_map TYPE HASHED TABLE OF lty_cache_map WITH UNIQUE KEY cache_id.
    CLASS-DATA gr_instance TYPE REF TO zcl_uitb_data_cache .
    DATA:
      mt_global_data   TYPE HASHED TABLE OF zuitb_global_data_ref WITH UNIQUE KEY field_name repid,
      mt_cache_map     TYPE ltt_cache_map,
      mv_current_repid TYPE sy-repid.

    "! <p class="shorttext synchronized" lang="en">Initializes the program cache</p>
    "!
    METHODS initialize_cache.
    "! <p class="shorttext synchronized" lang="en">Set's the current report id</p>
    "!
    "! @parameter iv_repid | <p class="shorttext synchronized" lang="en"></p>
    METHODS set_current_repid
      IMPORTING
         iv_repid TYPE sy-repid .
ENDCLASS.



CLASS ZCL_UITB_DATA_CACHE IMPLEMENTATION.


  METHOD clear_cache.
    FREE mt_global_data.
  ENDMETHOD.


  METHOD clear_object_ref.
    FIELD-SYMBOLS: <lr_object_ref> TYPE any.

    ASSIGN mt_global_data[ field_name = iv_registered_name ] TO FIELD-SYMBOL(<ls_global_data_ref>).
    IF sy-subrc = 0.
      ASSIGN <ls_global_data_ref>-field_ref->* TO <lr_object_ref>.
      IF sy-subrc = 0.
        CLEAR: <lr_object_ref>.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_data_ref.
    IF line_exists( mt_global_data[ field_name = iv_registered_name ] ).
      DATA(lr_data_ref) = REF #( mt_global_data[ field_name = iv_registered_name
                                                 repid      = mv_current_repid   ] ).
      rr_data_ref = lr_data_ref->field_ref.
    ELSE.
      MESSAGE |Variable { iv_registered_name } is not registered in the global data cache| TYPE 'A'.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    IF gr_instance IS INITIAL.
      gr_instance = NEW #( ).
    ENDIF.

    gr_instance->set_current_repid( iv_repid ).
    gr_instance->initialize_cache( ).

    rr_instance = gr_instance.
  ENDMETHOD.


  METHOD initialize_cache.
    " check if the cache is initialized
    CHECK NOT line_exists( mt_global_data[ repid = mv_current_repid ] ).

    PERFORM ('INIT_PROG_DATA_CACHE') IN PROGRAM (mv_current_repid) USING me.
  ENDMETHOD.


  METHOD is_cache_initialized.
    IF mt_global_data IS NOT INITIAL.
      rf_cache_is_filled = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD register_data.
    INSERT VALUE #( field_name = iv_variable_name
                    repid      = mv_current_repid
                    field_ref  = ir_variable_ref ) INTO TABLE mt_global_data.
  ENDMETHOD.


  METHOD set_current_repid.
    mv_current_repid = iv_repid.
  ENDMETHOD.


  METHOD update_object_ref.
    FIELD-SYMBOLS: <lr_object_ref> TYPE any.

    ASSIGN mt_global_data[ field_name = iv_registered_name ] TO FIELD-SYMBOL(<ls_global_data_ref>).
    IF sy-subrc = 0.
      ASSIGN <ls_global_data_ref>-field_ref->* TO <lr_object_ref>.
      IF sy-subrc = 0.
        <lr_object_ref> ?= ir_new_object_ref.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
