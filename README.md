## Tagging Modes of transport
This R Shiny application lets you quickliy label mode of transport of trips recorded by utrail-devices. 
It requires at least one file with Accelerometer information and one file with GPS information (including speed).
The application will then visualise location, speed and acceleration and offer a reasonably intuitive GUI to label the trips.
In the end, the files will be merged and made available for download.
I have provided some minutes of dummy data of myself to give you an idea of how it could work even if you do not have own utrail-devices.

## See it in action
The app is hosted on shinyapps.io under https://oliver-burkhard.shinyapps.io/17fs_movement_annotation_interface/

## Instructions
### Control panel (left hand side)
  - GPS ind IMU files *must* be provided in order for the application to work.
  - Uploading a flagging file will get you red lines where you manually set flags during recording
  - Uploading a labelled file (what you get if you download the results) will automatically apply the labels from the file to the current GPS and IMU files. Note that you can alter the labels without affecting the pre-labelled file.
  - Moving backwards or ahead will shift the window by half the window size (default: 10'). Center will reset the center of the current window at the currently selected point.
  - Zooming in or out will make the (time-)window bigger or smaller.
  - The *Remove rest* button will flagg all points currently flagged as *unknown* for elimination (no lipsync challenge). They will be deleted before downloading and thus saves space by not storing uninteresting parts of the trajectory. This may result in gaps.
  - Selecting a mode will immedialtely flag the currently selected point and the largest possible uniterrupted sequence of preceeding points with the same flag with the chosen mode. The idea is that the tagging happens chronologically: Select the last point of a given mode and label the whole subsequence.
  - Downloading the data is only possible if there is no remaining untagged point. You can always ensure this by pressing *Remove rest*
  
### Viewer pane
  - The first graph is an overview that spans the entire duration contained in the files. Labelling will remove the points from the "unknown" bottom line. The current window (based on IMU data) is marked by a gray background. The black lines (usually at the side of the grey rectangle denote the time of the first and last GPS point in the window)
  - Next up is a map displaying the GPS points falling in the window (plus some additional ones at the border to make sure the entire time could be covered). Blue points represent GPS measurements, the red point represents the currently selected point. The line over the points takes the colour of the chosen label.
  - Next is a map with the speed (as measured by GPS). There can be missing values. Grey lines in the background are at 4.5, 30, 50 and 80 km/h, which are relevant in the Swiss setting the data comes from.
  - Lastly we offer an overview of the Acceleration.
