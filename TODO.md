## ToDo list

* FIX: When window loses focus input freezes || onVisibilityChange doesn't work.
* DESIGN: Player representation as dynamic entity.
* FIX: Rendering resolution shouldn't scale. It's bigger than screen.
* ADD: resolveCollisions – function should move dynamic entities if they do not collide in given deltaTime. If they do collide, it should move rest of the entities in half of deltaTime twice and repeat recursively (till it reaches deltaTime < epsilon).
