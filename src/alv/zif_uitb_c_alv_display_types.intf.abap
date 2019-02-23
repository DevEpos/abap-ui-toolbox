INTERFACE zif_uitb_c_alv_display_types
  PUBLIC .

  "! <p class="shorttext synchronized" lang="en">ALV is shown in container of existing view</p>
  CONSTANTS embedded TYPE i VALUE 0.
  "! <p class="shorttext synchronized" lang="en">ALV is shown in a non modal dialog</p>
  CONSTANTS dialog TYPE i VALUE 1.
  "! <p class="shorttext synchronized" lang="en">ALV is shown in a modal dialog</p>
  CONSTANTS modal_dialog TYPE i VALUE 2.
ENDINTERFACE.
