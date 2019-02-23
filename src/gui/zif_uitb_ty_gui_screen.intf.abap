INTERFACE zif_uitb_ty_gui_screen
  PUBLIC .

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Map of F-Key to user function</p>
    "!
    BEGIN OF ty_s_fkey_map,
      fkey            TYPE ui_func,
      mapped_function TYPE ui_func,
      text            TYPE gui_text,
    END OF ty_s_fkey_map.

  TYPES: ty_t_fkey_map TYPE STANDARD TABLE OF ty_s_fkey_map WITH KEY fkey.
ENDINTERFACE.
