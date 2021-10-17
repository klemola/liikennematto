module Model.AnimationSchedule exposing
    ( AnimationSchedule
    , add
    , empty
    , toList
    , update
    )

import Duration exposing (Duration)
import Model.Animation as Animation exposing (Animation)


type AnimationSchedule
    = AnimationSchedule (List Animation)


empty : AnimationSchedule
empty =
    AnimationSchedule []


add : List Animation -> AnimationSchedule -> AnimationSchedule
add newAnimations (AnimationSchedule schedule) =
    AnimationSchedule (schedule ++ newAnimations)


update : Duration -> AnimationSchedule -> AnimationSchedule
update delta (AnimationSchedule schedule) =
    -- removes schedule when they are completed
    AnimationSchedule (schedule |> List.filterMap (Animation.update delta))


toList : AnimationSchedule -> List Animation
toList (AnimationSchedule schedule) =
    schedule
