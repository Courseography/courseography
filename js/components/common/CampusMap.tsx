import { type JSX, useRef } from "react"
import L from "leaflet"
import { FeatureGroup, MapContainer, Marker, Popup, TileLayer } from "react-leaflet"
import { convertTimeToString } from "./utils"
import type { BuildingGroup, LectureTimeframe } from "./types"

export interface CampusMapProps {
  lecturesByBuilding: BuildingGroup[]
  selectedLecTimeframes: LectureTimeframe[]
}

/**
 * Interactive Leaflet campus map displaying markers for lecture locations.
 *
 * Plots building markers based on scheduled lecture coordinates, highlights selected
 * buildings with custom icons, displays popups listing class meeting times, and provides
 * a "Zoom to Fit" control to dynamically adjust map bounds to encompass all active markers.
 *
 * @param lecturesByBuilding Array of buildings with their coordinates and scheduled classes.
 * @param selectedLecTimeframes Currently selected lectures to highlight on the map.
 */
export function CampusMap({
  lecturesByBuilding,
  selectedLecTimeframes,
}: CampusMapProps): JSX.Element {
  const mapRef = useRef<L.Map | null>(null)
  const groupRef = useRef<L.FeatureGroup | null>(null)

  const zoomToFit = (): void => {
    const map = mapRef.current
    const mapGroup = groupRef.current

    if (!map || !mapGroup) return

    // markerCount is the number of markers on the map.
    // Only adjust the map bounds if there are multiple markers because the zoom will
    // be strange for 1 marker. Note that the map will be centered on the single marker
    const markerCount = mapGroup.getLayers().length

    if (markerCount > 1) {
      map.fitBounds(mapGroup.getBounds(), { padding: [30, 30] })
    }
  }

  const blueMarker = new L.Icon({
    iconUrl: "/static/res/ico/blue-marker.png",
    shadowUrl: "/static/res/ico/shadow.png",
    iconAnchor: [5, 35],
    popupAnchor: [13, -35],
    iconSize: [34, 35],
    shadowSize: [50, 85],
    shadowAnchor: [2, 90],
  })

  const redMarker = new L.Icon({
    iconUrl: "/static/res/ico/red-marker.png",
    shadowUrl: "/static/res/ico/shadow.png",
    iconAnchor: [5, 35],
    popupAnchor: [13, -35],
    iconSize: [34, 35],
    shadowSize: [50, 85],
    shadowAnchor: [2, 90],
  })

  const locationMarkers = lecturesByBuilding.map(building => {
    if (building.lat === undefined || building.lng === undefined) {
      return null
    }

    const description = building.days
      .sort((dayA, dayB) => {
        return dayA.dayOrder - dayB.dayOrder
      })
      .map(day => {
        const dayTimes = day.timeframes
          .sort((timeA, timeB) => {
            return timeA.startTime - timeB.startTime
          })
          .map(time => (
            <li key={time.courseCode + "-" + time.startTime + "-" + time.endTime}>
              {convertTimeToString(time.startTime) +
                "-" +
                convertTimeToString(time.endTime) +
                ", " +
                time.room +
                ": " +
                time.courseCode}
            </li>
          ))

        return (
          <div key={day.dayString}>
            <p>{day.dayString}</p>
            {dayTimes}
          </div>
        )
      })

    let colouredMarker
    const buildingInd = selectedLecTimeframes.findIndex(
      lec => lec.location && lec.location.buildingCode === building.buildingCode
    )

    colouredMarker = buildingInd == -1 ? blueMarker : redMarker

    return (
      <Marker
        key={building.buildingName}
        icon={colouredMarker}
        position={[building.lat, building.lng]}
      >
        <Popup key={building.postalCode}>
          <p>
            <b> {building.buildingName + " | " + building.buildingCode} </b>
          </p>
          <p>
            <em>{building.address}</em>
          </p>
          {description}
        </Popup>
      </Marker>
    )
  })

  const mapBounds = L.latLngBounds([])

  // If there are selected lectures, center the map based on the locations of those lectures, otherwise, center the map
  // on Bahen
  let center: L.LatLngExpression = [43.6596426, -79.3976676]

  if (lecturesByBuilding.length) {
    lecturesByBuilding.forEach(building => {
      if (building.lat !== undefined && building.lng !== undefined) {
        mapBounds.extend(L.latLng(building.lat, building.lng))
      }
    })

    center = mapBounds.getCenter()
  }

  return (
    <div id="campus-map" className="col-md-8 col-6">
      <MapContainer
        center={center}
        zoom={16}
        maxZoom={40}
        attributionControl={true}
        zoomControl={true}
        doubleClickZoom={true}
        scrollWheelZoom={true}
        dragging={true}
        ref={mapRef}
      >
        <TileLayer url="http://{s}.tile.osm.org/{z}/{x}/{y}.png" />
        <FeatureGroup ref={groupRef}>{locationMarkers}</FeatureGroup>
      </MapContainer>
      <button id="zoom-button" onClick={zoomToFit}>
        Zoom to Fit
      </button>
    </div>
  )
}
