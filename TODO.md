## ToDo list

    - FIX: When window loses focus input stays. It should be cleared.
    - DESIGN: Player representation as dynamic entity.
    - ADD: updatePlayer only updates movementVector for now. It should change
    player's position
    - ADD: resolveCollisions – function should move dynamic entities if they do
    not collide in given deltaTime. If they do collide it should move rest of
    the entities in half of deltaTime twice and repeat recursively (till it
            reaches deltaTime < epsilon).
