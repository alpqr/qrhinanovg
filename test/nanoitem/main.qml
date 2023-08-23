import QtQuick
import NanoItemExample

Item {
    Rectangle {
        color: "lightGray"
        anchors.fill: parent

        TestNanoItem {
            x: 100
            y: 100
            width: 500
            height: 500
            NumberAnimation on rotation { from: 0; to: 360; duration: 5000; loops: -1 }
        }

        Rectangle {
            anchors.centerIn: parent
            width: 200
            height: 200
            color: "magenta"
            Text {
                text: "Some item on top.\nPress and move with mouse\nto move eyes."
                anchors.centerIn: parent
            }
        }
    }
}
