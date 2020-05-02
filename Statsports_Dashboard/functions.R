# set up a test df with corner, opposite corner, other corner 
# test_df <- data.frame(
#   lat = c(51.662238, 51.662348, 51.661415, 51.661639),
#   lng = c(-0.273112, -0.27268, -0.272495, -0.271635)
# )

# lookup of possible pitches - just use one for now for testing
pitch_lookup <- tribble(
  ~pitch_name, ~centre_goal_lat, ~centre_goal_lng, ~corner_1_lat, ~corner_1_lng, ~corner_2_lat, ~corner_2_lng, ~corner_3_lat, ~corner_3_lng,
  "pitch 1", 51.662348, -0.27268, 51.662238, -0.273112, 51.661415,-0.272495, 51.661639,-0.271635, 
  "pitch 2", 52.945244, -1.410825, 52.944974, -1.411007, 52.944642, -1.409686, 52.945173, -1.409312, 
  "pitch 3", 52.944688, -1.416016, 52.944421, -1.416183, 52.944106, -1.414803, 52.944643, -1.414450, 
  "pitch 4", 52.942615, -1.416661, 52.942676, -1.417142, 52.941761, -1.417466, 52.941636, -1.416499,
  "pitch 5", 52.944484, -1.411824, 52.944217, -1.412001, 52.943884, -1.410630, 52.944421, -1.410263, 
  "pitch 6", 52.942490, -1.415665, 52.942550, -1.416142, 52.941633, -1.416471, 2.941509, -1.415502, 
  "pitch 7", 52.944134, -1.416616, 52.944194, -1.417082, 52.943321, -1.417410, 52.943208, -1.416469, 
  "pitch 8", 52.942752, -1.417717, 52.942813, -1.418196, 52.941892, -1.418525, 52.941772, -1.417564,
  "pitch 9", 52.915320, -1.447651, 52.915159, -1.448066, 52.914336, -1.447156, 52.914645, -1.446327,
  "pitch 10", 53.284000, -6.373780, 53.283958, -6.374281, 53.283027, -6.374060, 53.283111, -6.373067,
  "pitch 11", 34.161823, -118.167701, 34.161787, -118.168020, 34.160802, -118.167892, 34.160879, -118.167214)


# functions
## degrees to radians conversion
deg2rad <- function(deg) {(deg * pi) / (180)}
rad2deg <- function(rad) {(rad * 180) / (pi)}

## calculate heading
get_heading <- function(lat_1, lat_2, lng_1, lng_2) {
  x <- cos(deg2rad(lat_1))*sin(deg2rad(lat_2)) - sin(deg2rad(lat_1))*cos(deg2rad(lat_2))*cos(deg2rad(lng_2)-deg2rad(lng_1))
  y <- sin(deg2rad(lng_2)-deg2rad(lng_1)) * cos(deg2rad(lat_2))
  head_rad <- atan2(y,x)
  rad2deg(head_rad)
}


## calculate distance 
get_dist <- function(lat_1, lat_2, lng_1, lng_2){
  pt1 <- cbind(lng_1,lat_1)
  pt2 <- cbind(lng_2,lat_2)
  geosphere::distGeo(pt1, pt2)
}

## get nearest pitch function to be created
get_nearest_pitch <- function(lat,lng,lookup){
  ## find the centroid of our points
  centroid_lat <- mean(lat,na.rm=TRUE)
  centroid_lng <- mean(lng, na.rm =TRUE)
  pt1 <- cbind(centroid_lng,centroid_lat)
  lookup_centre_lat <- lookup$centre_goal_lat
  lookup_centre_lng <- lookup$centre_goal_lng
  pt2 <- cbind(lookup_centre_lng,lookup_centre_lat)
  ## get distance to each pitch from our points
  dists <- geosphere::distGeo(pt1, pt2)
  lookup[which.min(dists),'pitch_name',TRUE]
}

#create function to rotate putch using lat lon values from nearest pitch and scale to 100x100 for ease of plotting
rotate_pitch <- function(lat, lng, lookup) {
  # get nearest pitch 
  pitch_name <- get_nearest_pitch(lat,lng,lookup)
  message('nearest pitch is ', pitch_name)
  pitch_coords <- lookup[lookup$pitch_name == pitch_name,]
  pitch_corner_1_lat <- pitch_coords$corner_1_lat 
  pitch_center_lat <- pitch_coords$centre_goal_lat
  pitch_corner_1_lng <- pitch_coords$corner_1_lng
  pitch_center_lng <- pitch_coords$centre_goal_lng
  rotation <- get_heading(pitch_corner_1_lat,pitch_center_lat,pitch_corner_1_lng,pitch_center_lng )
  r <- get_dist(pitch_corner_1_lat, lat, pitch_corner_1_lng, lng)
  heading <- get_heading(pitch_corner_1_lat, lat, pitch_corner_1_lng, lng)
  theta <- deg2rad(heading - rotation)
  x <- r*sin(theta)
  y <- r*cos(theta)
  # get rotation of lookup
  lookup_lats <- c(pitch_coords$corner_1_lat, pitch_coords$corner_2_lat, pitch_coords$corner_3_lat)
  lookup_lngs <- c(pitch_coords$corner_1_lng, pitch_coords$corner_2_lng, pitch_coords$corner_3_lng)
  r_lookup <- get_dist(pitch_corner_1_lat, lookup_lats, pitch_corner_1_lng, lookup_lngs)
  heading_lookup <- get_heading(pitch_corner_1_lat, lookup_lats, pitch_corner_1_lng, lookup_lngs)
  theta <- deg2rad(heading_lookup - rotation)
  x_lookup <- r_lookup*sin(theta)
  y_lookup <- r_lookup*cos(theta)
  length<- max(y_lookup)
  width <- max(x_lookup)
  # rescale x and y to be between 0 and 100
  x <- x/width * 100
  y <- y / length * 100
  return(data.frame(x,y))
}





