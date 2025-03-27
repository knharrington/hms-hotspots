# hms-hotspots

![image](https://github.com/user-attachments/assets/bca6e482-fa27-4349-bb94-17a6db8129d0)

### About this Project
Bycatch, the unintentional capture of species fishers don’t want, can’t sell, or can’t keep, can be a significant challenge in commercial fisheries. Beyond its environmental impact, bycatch creates economic inefficiencies and regulatory hurdles for the fishing industry. Bycatch issues became particularly pressing after the 2010 Deepwater Horizon oil spill, which disrupted Gulf Coast ecosystems and fisheries.

To restore impacted fisheries, the Communications Networks and Mapping Tools to Reduce Bycatch Project was initiated by the Deepwater Horizon Open Ocean Trustees in 2019. Mote Marine Laboratory is collaborating with the National Fish and Wildlife Foundation (NFWF) and National Oceanic and Atmospheric Administration (NOAA), who manage this project, with the goal of exploring how real-time communication networks can help reduce bycatch. By enabling fishermen to share and act on spatial data in near real-time, bycatch hotspots can be identified and avoided by a fleet of participants. As part of this initiative, a Shiny for R application has been developed to prototype a streamlined data reporting and visualization tool that can benefit commercial fishers in the Gulf of America.

Using Shiny, this prototype for an interactive web app, tailored to the needs of commercial fishermen within the highly migratory species fishery, was designed to facilitate real-time data sharing and visualization based on fisher-provided feedback. These fishermen have reported the need to avoid bluefin tuna while fishing. The app reflects this request to enable fishers to know to avoid the locations before they leave port or once they are out on the water. 

#### User-Submitted Reports
The app allows participating fishermen to submit real-time reports about bluefin tuna interactions and good fishing locations. This feature is user-friendly, with only two buttons to record either unfavorable or favorable fishing conditions. Upon submission, observations are automatically assigned a location utilizing geolocation services, as well as a user identifier and time stamp. The goal was to minimize the time and effort required to submit accurate reports.

#### Automated Quality Assurance
To ensure the integrity of the data, the app includes automated checks for common errors, such as out-of-bounds GPS coordinates. This feature helps maintain the reliability of the information being shared across the network.

#### Real-Time Mapping Solutions
Once data is submitted, the app sends it to a secure Google Sheet for storage and generates interactive maps that visualize bluefin tuna hotspots in near real-time. Users can visualize their group’s collective data in a 15 min by 15 min grid or a heat map and filter the entries by time frame, enabling them to identify patterns and make informed decisions quickly. Additionally, fishermen can view near real-time surface current information on the basemap.

### Visit the App
Visit the app [here](https://cfemm.shinyapps.io/hms-hotspots) using credentials (username: test-user; password: hotspots).
