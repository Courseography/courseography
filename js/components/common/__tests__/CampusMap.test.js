import { render, screen } from "@testing-library/react"
import userEvent from "@testing-library/user-event"
import { CampusMap } from "../CampusMap"

// Mock Leaflet to avoid DOM rendering issues in tests
jest.mock("leaflet", () => ({
  Icon: jest.fn(() => ({})),
  Map: jest.fn(),
  latLngBounds: jest.fn(() => ({
    extend: jest.fn(),
    getCenter: jest.fn(() => ({ lat: 43.6596426, lng: -79.3976676 })),
  })),
  latLng: jest.fn((lat, lng) => ({ lat, lng })),
}))

jest.mock("react-leaflet", () => ({
  MapContainer: ({ children }) => <div data-testid="map-container">{children}</div>,
  TileLayer: ({ url }) => <div data-testid="tile-layer" data-url={url} />,
  FeatureGroup: ({ children }) => <div data-testid="feature-group">{children}</div>,
  Marker: ({ position, children }) => (
    <div data-testid="marker" data-position={JSON.stringify(position)}>
      {children}
    </div>
  ),
  Popup: ({ children }) => <div data-testid="popup">{children}</div>,
}))

describe("CampusMap", () => {
  const mockLecturesByBuilding = [
    {
      buildingName: "Bahen Centre for Information Technology",
      buildingCode: "BA",
      address: "40 St George St",
      lat: 43.6596426,
      lng: -79.3976676,
      postalCode: "M5S 2E4",
      days: [
        {
          dayString: "Monday",
          dayOrder: 0,
          timeframes: [
            {
              courseCode: "CSC309H1",
              session: "L5101",
              startTime: 11,
              endTime: 13,
              room: "BA3200",
            },
          ],
        },
      ],
    },
  ]

  it("renders the map container", () => {
    render(
      <CampusMap lecturesByBuilding={mockLecturesByBuilding} selectedLecTimeframes={[]} />
    )

    const mapContainer = screen.getByTestId("map-container")
    expect(mapContainer).toBeTruthy()
  })

  it("renders markers for each building", () => {
    render(
      <CampusMap lecturesByBuilding={mockLecturesByBuilding} selectedLecTimeframes={[]} />
    )

    const markers = screen.getAllByTestId("marker")
    expect(markers).toHaveLength(1)
  })

  it("renders multiple markers for multiple buildings", () => {
    const multipleBuildings = [
      ...mockLecturesByBuilding,
      {
        buildingName: "Myhal Centre",
        buildingCode: "MY",
        address: "55 St George St",
        lat: 43.6597,
        lng: -79.3975,
        postalCode: "M5S 2E5",
        days: [
          {
            dayString: "Tuesday",
            dayOrder: 1,
            timeframes: [
              {
                courseCode: "CSC301H1",
                session: "L0101",
                startTime: 14,
                endTime: 16,
                room: "MY150",
              },
            ],
          },
        ],
      },
    ]

    render(
      <CampusMap lecturesByBuilding={multipleBuildings} selectedLecTimeframes={[]} />
    )

    const markers = screen.getAllByTestId("marker")
    expect(markers).toHaveLength(2)
  })

  it("renders Zoom to Fit button", () => {
    render(
      <CampusMap lecturesByBuilding={mockLecturesByBuilding} selectedLecTimeframes={[]} />
    )

    const zoomButton = screen.getByText("Zoom to Fit")
    expect(zoomButton).toBeTruthy()
  })

  it("zoom button is clickable", async () => {
    const user = userEvent.setup()
    render(
      <CampusMap lecturesByBuilding={mockLecturesByBuilding} selectedLecTimeframes={[]} />
    )

    const zoomButton = screen.getByText("Zoom to Fit")
    await user.click(zoomButton)

    // Button should not throw errors when clicked
    expect(zoomButton).toBeTruthy()
  })

  it("does not render markers for buildings without coordinates", () => {
    const buildingsWithoutCoords = [
      {
        buildingName: "Unknown Building",
        buildingCode: "UNK",
        address: "Unknown",
        lat: undefined,
        lng: undefined,
        postalCode: "M5S 2E4",
        days: [],
      },
    ]

    render(
      <CampusMap lecturesByBuilding={buildingsWithoutCoords} selectedLecTimeframes={[]} />
    )

    const markers = screen.queryAllByTestId("marker")
    expect(markers).toHaveLength(0)
  })

  it("renders popup component for markers", () => {
    render(
      <CampusMap lecturesByBuilding={mockLecturesByBuilding} selectedLecTimeframes={[]} />
    )

    // Check that popup component is rendered
    const popups = screen.getAllByTestId("popup")
    expect(popups.length).toBeGreaterThan(0)
  })
})
