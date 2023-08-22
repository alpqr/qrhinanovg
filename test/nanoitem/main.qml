import QtQuick
import NanoItemExample

Item {
    Rectangle {
        color: "lightGray"
        anchors.fill: parent

        TestNanoItem {
            width: 300
            height: 300
            anchors.centerIn: parent
        }

        Rectangle {
            anchors.centerIn: parent
            width: 200
            height: 200
            color: "green"
            NumberAnimation on rotation { from: 0; to: 360; duration: 3000; loops: -1 }
        }
    }
}
