# Liikennematto

Liikennematto (_Finnish for a "traffic mat"_) is a prototype traffic simulation with a tiny scale. Inspired by traffic mats that children play with.

![Screenshot](docs/screenshot.png)

_Liikennematto in action! [Live demo](http://apps.butsku.com/liikennematto/)_

## Features

### Simulation

-   a two-lane road network
-   collision prevention
-   signal intersections (traffic lights)
-   yield sign based intersections
-   dead end streets
-   prodedural generation of lots and buildings
-   debug tools
    -   pause simulation
    -   spawn cars
    -   road network visualization
    -   road network DOT string export
    -   car debug layers

### Map editor

-   draw roads of all kind with the smart editor
    -   **left click / tap** to place a piece of road
    -   **right click** to remove road from the tile
-   bulldoze (_remove_) tiles [_for touch devices_]
-   blow up the board (_clear it_) with dynamite

#### Restrictions

-   tiles cannot be placed everywhere. In order to avoid awkward road layouts, the editor restricts complexity
-   lots are only placed next to road tiles that have enough distance to intersections, curves and dead ends

## Demo & more information

> Liikennematto works best on a large screen with landscape orientation (iPad, laptops, desktops)

[Live demo (stable)](http://apps.butsku.com/liikennematto/)

[Preview version (often in sync with the master branch)](http://apps.butsku.com/liikennematto/next/)

---

Read more about the project from [Liikennematto dev blog #1: prototyping traffic simulation with Elm](https://matiasklemola.com/liikennematto-dev-blog-one)

[Part #2](https://matiasklemola.com/liikennematto-devlog-two), [Part #3](https://matiasklemola.com/liikennematto-devlog-three), [Part #4](https://matiasklemola.com/liikennematto-devlog-four)

---

[Follow me on Twitter](https://twitter.com/MatiasKlemola) for updates!

## Attribution

Thanks to [Kenney](https://kenney.nl/assets) for the free game assets (subset of the "Road Textures" and "Racing Pack" collections).

Icons made by <a href="https://www.flaticon.com/authors/smashicons" title="Smashicons">Smashicons</a> from <a href="https://www.flaticon.com/" title="Flaticon"> www.flaticon.com</a>
