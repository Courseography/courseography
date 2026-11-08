import { render, screen } from "@testing-library/react"
import { CampusMap } from "../CampusMap"

// Mock Leaflet to avoid DOM rendering issues in tests
jest.mock("leaflet", () => ({
  Icon: jest.fn(() => ({})),
  latLngBounds: jest.fn(() => ({
    extend: jest.fn(),
    getCenter: jest.fn(() => ({ lat: 43.6596426, lng: -79.3976676 })),
  })),
  latLng: jest.fn((lat, lng) => ({ lat, lng })),
}))

/* eslint-disable react/prop-types */
jest.mock("react-leaflet", () => ({
  MapContainer: ({ children, maxZoom }) => (
    <div data-testid="map-container" data-max-zoom={maxZoom}>
      {children}
    </div>
  ),
  TileLayer: ({ url, attribution, maxZoom }) => (
    <div
      data-testid="tile-layer"
      data-url={url}
      data-attribution={attribution}
      data-max-zoom={maxZoom}
    />
  ),
  FeatureGroup: ({ children }) => <div data-testid="feature-group">{children}</div>,
  Marker: ({ children }) => <div data-testid="marker">{children}</div>,
  Popup: ({ children }) => <div data-testid="popup">{children}</div>,
}))
/* eslint-enable react/prop-types */

// Checks the tile layer settings required by the OpenStreetMap tile usage policy:
// https://operations.osmfoundation.org/policies/tiles/
describe("CampusMap OpenStreetMap tile layer", () => {
  beforeEach(() => {
    render(<CampusMap lecturesByBuilding={[]} selectedLecTimeframes={[]} />)
  })

  it("uses the canonical OpenStreetMap tile URL", () => {
    const tileLayer = screen.getByTestId("tile-layer")
    expect(tileLayer.dataset.url).toBe("https://tile.openstreetmap.org/{z}/{x}/{y}.png")
  })

  it("credits OpenStreetMap and links to its copyright page", () => {
    const attribution = screen.getByTestId("tile-layer").dataset.attribution
    expect(attribution).toContain('href="https://www.openstreetmap.org/copyright"')
    expect(attribution).toContain("OpenStreetMap</a> contributors")
  })

  it("links to the OpenStreetMap report a map issue page", () => {
    const attribution = screen.getByTestId("tile-layer").dataset.attribution
    expect(attribution).toContain('href="https://www.openstreetmap.org/fixthemap"')
  })

  it("limits the map and tile layer to OpenStreetMap's maximum zoom level", () => {
    expect(screen.getByTestId("tile-layer").dataset.maxZoom).toBe("19")
    expect(screen.getByTestId("map-container").dataset.maxZoom).toBe("19")
  })
})
