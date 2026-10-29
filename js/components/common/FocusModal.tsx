import { type JSX, useEffect, useState } from "react"
import ReactModal from "react-modal"
import { getProgram } from "./utils"
import type { ProgramData, ProgramNotModified } from "./types"

interface FocusModalProps {
  showFocusModal: boolean
  focusId: string
  onClose: () => void
}

/**
 * Modal popup that displays the description and requirements of a computer science focus.
 *
 * Fetches focus data asynchronously from the server using conditional caching
 * (`If-Modified-Since`) and displays the department title, focus description,
 * and program completion requirements.
 *
 * @param showFocusModal Whether the focus modal popup is currently visible.
 * @param focusId The unique program code identifier of the focus.
 * @param onClose Callback invoked when the modal is closed.
 */
export function FocusModal({
  showFocusModal,
  focusId,
  onClose,
}: FocusModalProps): JSX.Element {
  const [focusTitle, setFocusTitle] = useState<string>("")
  const [focusModifiedTime, setFocusModifiedTime] = useState<string>(
    new Date(0).toUTCString()
  )
  const [focusDescription, setFocusDescription] = useState<string>("")
  const [focusRequirements, setFocusRequirements] = useState<string>("")

  useEffect(() => {
    if (showFocusModal) {
      getProgram(focusId, focusModifiedTime).then(
        (focusData: ProgramData | ProgramNotModified) => {
          if (!focusData.modified) return

          setFocusTitle(focusData.title)
          setFocusDescription(focusData.description)
          setFocusRequirements(focusData.requirements)
          setFocusModifiedTime(focusData.modifiedTime ?? "")
        }
      )
    }
  }, [showFocusModal, focusId])

  return (
    <ReactModal
      className="modal-class"
      overlayClassName="overlay"
      isOpen={showFocusModal}
      onRequestClose={onClose}
      ariaHideApp={false}
    >
      <div className="modal-header">{focusTitle}</div>
      <div className="modal-body">
        <p dangerouslySetInnerHTML={{ __html: focusDescription }}></p>
        <p dangerouslySetInnerHTML={{ __html: focusRequirements }}></p>
      </div>
    </ReactModal>
  )
}
