# ABAP - Dynamic selection screen
A fully dynamic selection screen with FREE_SELECTION function modules in SAP ABAP 7.50

The situation: We wanted a report with a selection screen for a large application that would allow users to restrict data, given that the application handles a lot of it. In the old version of the report, we created the selection screen manually, adding boxes and icons to indicate status. However, as the application grew, our customer wanted to include Z-fields in the database and select data by Z-field. Some of these fields were transferred to our standard application, but some customers did not need them. Therefore, we manually added these fields to the selection screen and removed them at runtime if they were not needed.

With the new implementation of the report, we would like to have a dynamic selection screen that can grow with our software and customer wishes. SAP already has such an option: FREE_SELECTION_DIALOG.

However, if you use the FREE_SEL* function modules, you must accept certain limitations. Adding this to a selection screen is easy — just Google it — but adding your own touch, such as icons for the status, will cause issues. The FREE_SELECTION_DIALOG, on the other hand, has an AS_SUBSCREEN parameter. This sub-screen can be added to the selection screen along with other own sub-selectino-screens.

In this example, we will use the FREE_SEL* function modules to select EKKO (purchase order header data) and add an additional field for the maximum number of rows selected — our own touch, in this case.
