package provide gtfs 1.0
package require csv

namespace eval ::gtfs {
    namespace export schema table get
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema::agency {
    variable definition {
        agency_id {T varchar(16) ? {[dict exists $selection agency_id] ? {P 1} : {P 0}}}
        agency_name {R 1 T varchar(255) ? {![dict exists $selection agency_id] ? {P 1} : {P 0}}}
        agency_url {R 1 T varchar(255)}
        agency_timezone {R 1 T varchar(64)}
        agency_lang {T char(2)}
        agency_phone {T varchar(64)}
        agency_fare_url {T varchar(255)}
    }
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table::agency {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema::stops {
    variable definition {
        stop_id {R 1 T int P 1}
        stop_code {T int}
        stop_name {R 1 T varchar(255)}
        stop_desc {T varchar(255)}
        stop_lat {R 1 T float(10,6)}
        stop_lon {R 1 T float(10,6)}
        zone_id {T int}
        stop_url {T varchar(255)}
        location_type {T int}
        parent_station {T int}
        stop_timezone {T varchar(64)}
        wheelchair_boarding {T int}
    }
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table::stops {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema::routes {
    variable definition {
        route_id {R 1 T int P 1}
        agency_id {T varchar(16) -> agency(agency_id)}
        route_short_name {R 1 T varchar(255)}
        route_long_name {R 1 varchar(255)}
        route_desc {T varchar(255)}
        route_type {R 1 T int}
        route_url {T varchar(255)}
        route_color {T varchar(6)}
        route_text_color {T varchar(6)}
    }
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table::routes {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema::trips {
    variable definition {
        route_id {R 1 T int -> routes(route_id)}
        service_id {R 1 T int -> calendar(service_id)}
        trip_id {R 1 T int P 1}
        trip_headsign {T varchar(255)}
        trip_short_name {T varchar(255)}
        direction_id {T int}
        block_id {T int}
        shape_id {T int -> shapes(shape_id)}
        wheelchair_accessible {T int}
        bikes_allowed {T int}
    }
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table::trips {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema::times {
    variable definition {
        trip_id {R 1 P 1 T int -> trips(trip_id)}
        arrival_time {R 1 T time}
        departure_time {R 1 T time}
        stop_id {R 1 T int -> stops(stop_id)}
        stop_sequence {R 1 P 1 T int}
        stop_headsign {T varchar(255)}
        pickup_type {T int}
        drop_off_type {T int}
        shape_dist_traveled {T float(10,6)}
    }
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table::times {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema::calendar {
    variable definition {
        service_id {R 1 T int}
        monday {R 1 T int}
        tuesday {R 1 T int}
        wednesday {R 1 T int}
        thursday {R 1 T int}
        friday {R 1 T int}
        saturday {R 1 T int}
        sunday {R 1 T int}
        start_date {R 1 T date}
        end_date {R 1 T date}
    }
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table::calendar {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::schema::transfers {
    variable definition {
        from_stop_id {R 1 T int -> stops(stop_id)}
        to_stop_id {R 1 T int -> stops(stop_id)}
        transfer_type {R 1 T int}
        min_transfer_time {T int}
    }
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::table::transfers {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::get {
    namespace export *
    namespace ensemble create -parameters db
}

namespace eval ::gtfs::parse {
    namespace export *
    namespace ensemble create
}

namespace eval ::gtfs::meta {
    namespace export *
    namespace ensemble create
}

namespace eval ::gtfs::meta::schema {
    namespace export *
    namespace ensemble create
}

namespace eval ::gtfs::meta::table {
    namespace export *
    namespace ensemble create
}

namespace eval ::gtfs::meta::entity {
    namespace export *
    namespace ensemble create
}

::oo::class create ::gtfs::parse::match

::oo::define ::gtfs::parse::match constructor {} {
    my reset
}
    
::oo::define ::gtfs::parse::match method consume {arg} {
    my variable state
    my $state $arg
}
    
::oo::define ::gtfs::parse::match method end {} {
    my variable state
    switch -exact -- $state {
        L -
        EL {
            return -code error "expected argument after -like"
        }
        Q -
        EQ {
            return -code error "expected argument after -equals"
        }
        E {
            return -code error "expected -like or -equals after -exact"
        }
    }
    set state T
}
    
::oo::define ::gtfs::parse::match method equals {args} {
    my variable state equals
    if {$state eq {T}} then {
        return $equals
    } else {
        return -code error "can't query result of unterminated parser"
    }
}
    
::oo::define ::gtfs::parse::match method like {args} {
    my variable state like
    if {$state eq {T}} then {
        return $like
    } else {
        return -code error "can't query result of unterminated parser"
    }
}
    
::oo::define ::gtfs::parse::match method reset {} {
    my variable state like equals
    set state S
    set like {}
    set equals {}
}
    
::oo::define ::gtfs::parse::match method S {arg} {
    my variable state
    switch -exact -- $arg {
        -exact  {set state E}
        -like   {set state L}
        -equals {set state Q}
        default {
            return -code error "bad option: \"$arg\": expected -exact, -like, or -equals"
        }
    }
}
    
::oo::define ::gtfs::parse::match method E {arg} {
    my variable state
    switch -exact -- $arg {
        -like {set state EL}
        -equals {set state EQ}
        default {
            return -code error "bad option: \"$arg\": expected -like, or -equals"
        }
    }
}
    
::oo::define ::gtfs::parse::match method L {arg} {
    my variable state like
    foreach pattern $arg {
        if {$pattern ni $like} then {
            lappend like $pattern
        }
    }
    set state S
}
    
::oo::define ::gtfs::parse::match method Q {arg} {
    my variable state equals
    foreach literal $arg {
        if {$literal ni $equals} then {
            lappend equals $literal
        }
    }
    set state S
}
    
::oo::define ::gtfs::parse::match method EL {arg} {
    my variable state like
    if {$arg ni $like} then {
        lappend like $arg
    }
    set state S
}

::oo::define ::gtfs::parse::match method EQ {arg} {
    my variable state equals
    if {$arg ni $equals} then {
        lappend equals $arg
    }
    set state S
}
    
::oo::define ::gtfs::parse::match method T {} {
    return -code error "parsing terminated. reset parset first"
}

::oo::class create ::gtfs::parse::up

::oo::define ::gtfs::parse::up constructor {definition} {
    my variable def
    set def $definition
    my reset
}
    
::oo::define ::gtfs::parse::up method consume {arg} {
    my variable state
    my $state $arg
}
    
::oo::define ::gtfs::parse::up method safe {} {
    my variable state safe
    if {$state eq {T}} then {
        return $safe
    } else {
        return -code error "can't query result of unterminated parser"
    }
}
    
::oo::define ::gtfs::parse::up method cascade {} {
    my variable state cascade
    if {$state eq {T}} then {
        return $cascade
    } else {
        return -code error "can't query result of unterminated parser"
    }
}
    
::oo::define ::gtfs::parse::up method selected {} {
    my variable state selection
    if {$state eq {T}} then {
        return $selection
    } else {
        return -code error "can't query result of unterminated parser"
    }
}
    
::oo::define ::gtfs::parse::up method end {} {
    my variable state
    switch -exact -- $state {
        W {
            return -code error "expected argument after -weak"
        }
        I {
            return -code error "expected argument after -include"
        }
    }
    set state T
}
    
::oo::define ::gtfs::parse::up method reset {} {
    my variable state safe cascade selection
    set state S
    set safe 0
    set cascade 0
    set selection {}
}
    
::oo::define ::gtfs::parse::up method S {arg} {
    my variable state safe cascade
    switch -exact -- $arg {
        -safe {set safe 1}
        -cascade {set cascade 1}
        -weak {set state W}
        -include {set state I}
        default {
            return -code error "bad option \"$arg\": expected -safe, -cascade, -weak or -include"
        }
    }
}
    
::oo::define ::gtfs::parse::up method W {arg} {
    my variable state selection def
    foreach column $arg {
        if {![dict exists $def $column ->]} then {
            return -code error "invalid weakly-referenced column: $column"
        }
        dict set selection $column 0
    }
    set state S
}
    
::oo::define ::gtfs::parse::up method I {arg} {
    my variable state selection def
    foreach column $arg {
        if {![dict exists $def $column]} then {
            return -code error "invalid column: $column"
        }
        if {![dict exists $def $column R] || ![dict get $def $column R]} then {
            dict set selection $column 1
        }
    }
    set state S
}

proc ::gtfs::meta::schema::up {db name definition arguments} {
    set parser [::gtfs::parse::up new $definition]
    try {
        foreach arg $arguments {
            $parser consume $arg
        }
        $parser end
        set selection [$parser selected]
        set safe [$parser safe]
        set cascade [$parser cascade]
    } finally {
        $parser destroy
    }
    
    if {$safe} then {
        set safe {if not exists}
    } else {
        set safe {}
    }
    
    if {$cascade} then {
        set cascade {on delete cascade}
    } else {
        set cascade {}
    }
    
    set key {}
    set constraints {}
    set members {}
    
    dict for {column properties} $definition {
        if {[dict exists $properties ?]} then {
            set properties [dict merge [dict remove $properties ?] [expr [dict get $properties ?]]]
        }
        set required [expr {[dict exists $properties R] && [dict get $properties R]}]
        set selected [dict exists $selection $column]
        if {$selected} then {
            set strong [dict get $selection $column]
        }
        set linked [dict exists $properties ->]
        if {$linked} then {
            set target [dict get $properties ->]
        }
        if {$required || $selected} then {
            lappend members [concat $column [dict get $properties T]]
            if {[dict exists $properties P] && [dict get $properties P]} then {
                lappend key $column
            }
        }
        if {$linked && (($required && (!$selected || $strong)) || ($selected && $strong))} then {
            lappend constraints [concat foreign key ($column) references $target $cascade]
        }
    }
    
    if {$key ne {}} then {
        lappend constraints [concat primary key ([join $key ,])]
    }
    
    set sql "create table $safe $name ("
    if {$members ne {}} then {
        append sql [join $members ,] 
    }
    if {$constraints ne {}} then {
        if {$members ne {}} then {
            append sql ,
        }
        append sql [join $constraints ,]
    }
    append sql ")"
    
    set statement [$db prepare $sql]
    try {
        set resultset [$statement execute]
        try {set results [$resultset allrows]} finally {$resultset close}
    } finally {
        $statement close
    }
    
    return $results
}
    
proc ::gtfs::meta::schema::down {db name args} {
    set safe 0
    while {$args ne {}} {
        switch -exact -- [lindex $args 0] {
            -safe {
                set args [lreplace $args 0 0]
                if {$args eq {}} then {
                    set safe 1
                } else {
                    set safe [lindex $args 0]
                    set args [lreplace $args 0 0]
                }
            }
            default {
                return -code error "bad option \"[lindex $args 0]\": expected -safe or none"
            }
        }
    }
    
    if {$safe} then {
        set sql "drop table if exists $name"
    } else {
        set sql "drop table $name"
    }
    
    set statement [$db prepare $sql]
    try {
        set resultset [$statement execute]
        try {set results [$resultset allrows]} finally {$resultset close}
    } finally {
        $statement close
    }
    
    return $results
}
    
proc ::gtfs::meta::schema::describe {db name} {
    set statement [$db prepare "desc $name"]
    try {
        set resultset [$statement execute]
        try {set results [$resultset allrows]} finally {$resultset close}
    } finally {
        $statement close
    }
    return $results
}

proc ::gtfs::meta::table::import {db name chan {fn {}}} {
    set columns [lmap entry [::gtfs::meta::schema::describe $db $name] {dict get $entry Field}]
    if {[gets $chan line] == -1} then {
        return -code error "couldn't read csv header"
    }
    
    set header [::csv::split $line]
    set missing {}

    foreach column $columns {
        if {$column ni $header} then {
            lappend missing $column
        }
    }
    
    if {$missing ne {}} then {
        return -code error "missing columns in header: $missing"
    }
    
    set sql "insert into $name values([join [lmap column $columns {format :%s $column}] ,])"
    
    set statement [$db prepare $sql]
    try {
        while {[gets $chan line] != -1} {
            set row [::csv::split $line]
            set index 0
            foreach column $header {
                if {$column ni $columns} then {
                    set $column [lindex $row $index]
                }
                incr index
            }
            set resultset [$statement execute]
            $resultset close
            if {$fn ne {}} then {
                uplevel 1 [list {*}$fn $row]
            }
        }
    } finally {
        $statement close
    }
}

proc ::gtfs::schema::agency::up {db args} {
    variable definition
    ::gtfs::meta::schema::up $db agency $definition $args
}

proc ::gtfs::schema::agency::down {db args} {
    ::gtfs::meta::schema::down $db agency {*}$args
}

proc ::gtfs::schema::agency::describe {db} {
    ::gtfs::meta::schema::describe $db agency
}

proc ::gtfs::table::agency::import {db args} {
    uplevel 1 ::gtfs::meta::table::import $db agency $args
}

proc ::gtfs::schema::stops::up {db args} {
    variable definition
    ::gtfs::meta::schema::up $db stops $definition $args
}

proc ::gtfs::schema::stops::down {db args} {
    ::gtfs::meta::schema::down $db stops {*}$args
}

proc ::gtfs::schema::stops::describe {db} {
    ::gtfs::meta::schema::describe $db stops
}

proc ::gtfs::table::stops::import {db args} {
    uplevel 1 ::gtfs::meta::table::import $db stops $args
}

proc ::gtfs::schema::routes::up {db args} {
    variable definition
    ::gtfs::meta::schema::up $db routes $definition $args
}

proc ::gtfs::schema::routes::down {db args} {
    ::gtfs::meta::schema::down $db routes {*}$args
}

proc ::gtfs::schema::routes::describe {db} {
    ::gtfs::meta::schema::describe $db routes
}

proc ::gtfs::table::routes::import {db args} {
    uplevel 1 ::gtfs::meta::table::import $db routes $args
}

proc ::gtfs::schema::trips::up {db args} {
    variable definition
    ::gtfs::meta::schema::up $db trips $definition $args
}

proc ::gtfs::schema::trips::down {db args} {
    ::gtfs::meta::schema::down $db trips {*}$args
}

proc ::gtfs::schema::trips::describe {db} {
    ::gtfs::meta::schema::describe $db trips
}

proc ::gtfs::table::routes::import {db args} {
    uplevel 1 ::gtfs::meta::routes::import $db trips $args
}

proc ::gtfs::schema::times::up {db args} {
    variable definition
    ::gtfs::meta::schema::up $db stop_times $definition $args
}

proc ::gtfs::schema::times::down {db args} {
    ::gtfs::meta::schema::down $db stop_times {*}$args
}

proc ::gtfs::schema::times::describe {db} {
    ::gtfs::meta::schema::describe $db stop_times
}

proc ::gtfs::table::times::import {db args} {
    uplevel 1 ::gtfs::meta::table::import $db stop_times $args
}

proc ::gtfs::schema::calendar::up {db args} {
    variable definition
    ::gtfs::meta::schema::up $db calendar $definition $args
}

proc ::gtfs::schema::calendar::down {db args} {
    ::gtfs::meta::schema::down $db calendar {*}$args
}

proc ::gtfs::schema::calendar::describe {db} {
    ::gtfs::meta::schema::describe $db calendar
}

proc ::gtfs::table::calendar::import {db args} {
    uplevel 1 ::gtfs::meta::table::import $db calendar $args
}

proc ::gtfs::schema::transfers::up {db args} {
    variable definition
    ::gtfs::meta::schema::up $db transfers $definition $args
}

proc ::gtfs::schema::transfers::down {db args} {
    ::gtfs::meta::schema::down $db transfers {*}$args
}

proc ::gtfs::schema::transfers::describe {db} {
    ::gtfs::meta::schema::describe $db transfers
}

proc ::gtfs::table::transfers::import {db args} {
    uplevel 1 ::gtfs::meta::table::import $db transfers $args
}

proc ::gtfs::get::proximity {db distance latitude longitude args} {
    set parser [::gtfs::parse::match new]
    try {
        foreach arg $args {
            $parser consume $arg
        }
        $parser end
        set values [$parser equals]
        set patterns [$parser like]
    } finally {
        $parser destroy
    }

    set sql {
        select * from (select *, 111.1111 * degrees(acos(cos(radians(stops.stop_lat)) *
                                            cos(radians(:latitude)) *
                                            cos(radians(stops.stop_lon - :longitude)) +
                                            sin(radians(stops.stop_lat)) *
                                            sin(radians(:latitude)))) as distance_in_km from stops) t
        where t.distance_in_km <= :distance
    }
    
    for {set index 0} {$index < [llength $values]} {incr index} {
        set value$index [lindex $values $index]
        lset values $index "t.stop_name = :value$index"
    }
    
    for {set index 0} {$index < [llength $patterns]} {incr index} {
        set pattern$index [lindex $patterns $index]
        lset patterns $index "t.stop_name like :pattern$index"
    }
    
    if {$values ne {} || $patterns ne {}} then {
        set sql "$sql and ([join [concat $values $patterns] { or }])"
    }
    
    set sql "$sql order by t.distance_in_km asc"
    
    set statement [$db prepare $sql]
    try {
        set resultset [$statement execute]
        try {set results [$resultset allrows]} finally {$resultset close}
    } finally {
        $statement close
    }
    
    return $results
}

proc ::gtfs::get::connections {db id args} {
    set columns {stop_times.*}
    set tables {stop_times}
    set clauses {}
    foreach arg $args {
        if {$arg ne {stop_id} && [dict exists $::gtfs::schema::stops::definition $arg]} then {
            if {"stops.$arg" ni $columns} then {
                lappend columns "stops.$arg"
            }
            if {{stops} ni $tables} then {
                lappend tables stops
                lappend clauses {stop_times.stop_id = stops.stop_id}
            }
        } elseif {$arg ne {trip_id} && [dict exists $::gtfs::schema::trips::definition $arg]} then {
            if {"trips.$arg" ni $columns} then {
                lappend columns "trips.$arg"
            }
            if {{trips} ni $tables} then {
                lappend tables trips
                lappend clauses {stop_times.trip_id = trips.trip_id}
            }
        } elseif {[dict exists $::gtfs::schema::routes::definition $arg]} then {
            if {"routes.$arg" ni $columns} then {
                lappend columns "routes.$arg"
            }
            if {{trips} ni $tables} then {
                lappend tables trips
                lappend clauses {stop_times.trip_id = trips.trip_id}
            }
            if {{routes} ni $tables} then {
                lappend tables routes
                lappend clauses {trips.route_id = routes.route_id}
            }
        } elseif {[dict exists $::gtfs::schema::routes::definition $arg]} then {
            if {"agency.$arg" ni $columns} then {
                lappend columns "agency.$arg"
            }
            if {{trips} ni $tables} then {
                lappend tables trips
                lappend clauses {stop_times.trip_id = trips.trip_id}
            }
            if {{routes} ni $tables} then {
                lappend tables routes
                lappend clauses {trips.route_id = routes.route_id}
            }
            if {{agency} ni $tables} then {
                lappend tables agency
                lappend clauses {routes.agency_id = agency.agency_id}
            }
        } else {
            return -code error "bad column \"$arg\": must be one of\
                                [dict keys [dict remove $::gtfs::schema::stops::definition stop_id]]\
                                [dict keys [dict remove $::gtfs::schema::trips::definition trip_id]]\
                                [dict keys [dict remove $::gtfs::schema::routes::definition route_id]]\
                                [dict keys [dict remove $::gtfs::schema::agency::definition agency_id]]"
        }
    }
    
    set sql "select [join $columns {, }] from [join $tables {, }] where stop_times.stop_id = :id"
    if {$clauses ne {}} then {
        set sql "$sql and [join $clauses { and }]"
    }
    
    set statement [$db prepare $sql]
    try {
        set resultset [$statement execute]
        try {set results [$resultset allrows]} finally {$resultset close}
    } finally {
        $statement close
    }
    
    return $results
}


