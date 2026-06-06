# GDScript syntax highlighting test
extends Node2D

class_name PlayerController

signal health_changed(amount: int)

const SPEED := 320.0
var health: int = 100
var inventory := ["sword", "potion"]

func _ready() -> void:
    health_changed.emit(health)

func _physics_process(delta: float) -> void:
    var direction := Input.get_vector("left", "right", "up", "down")
    position += direction * SPEED * delta

    if health <= 0:
        queue_free()
    elif direction.length() > 0:
        print("moving")
    else:
        pass
