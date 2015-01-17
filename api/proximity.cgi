#!/usr/bin/env tclsh

package require tdbc::mysql
package require ncgi
package require json::write
package require gtfs

#
# DB Configuration
#
set DBUSER {}
set DBPASSWORD {}
set DBHOST {}
set DB {}

tdbc::mysql::connection create db -user $USER -password $DBPASSWORD -db $DB -host $DBHOST

::ncgi::parse

::ncgi::header

set result [gtfs db get proximity [::ncgi::value distance] \
                                  [::ncgi::value lat] \
                                  [::ncgi::value lon] \
                                  -like [split [::ncgi::value like {}] {|}] \
                                  -equals [split [::ncgi::value equals {}] {|}]]

puts [::json::write array {*}[lmap row $result {
    ::json::write object {*}[dict map {key value} $row {
        if {![string is double -strict $value]} then {
            set value [::json::write string $value]
        } else {
            set value $value
        }
    }]
}]]

db close

