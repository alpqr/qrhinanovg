import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import NanoItemExample

Item {
    Rectangle {
        color: "lightGray"
        anchors.fill: parent

        TestNanoItem {
            x: 100
            y: 100
            width: 1000
            height: 600
            blowUp: cbBlowUp.checked
            NumberAnimation on rotation { from: 0; to: 360; duration: 15000; loops: -1; running: cbRotate.checked }
            NumberAnimation on t { from: 0; to: 100; duration: 50000 }
        }
    }

    ColumnLayout {
        CheckBox {
            id: cbRotate
            text: "Rotate"
            checked: true
        }
        CheckBox {
            id: cbBlowUp
            text: "Blow up"
            checked: false
        }
    }
}
