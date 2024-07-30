object Q1 extends App {
    case class Product(var id: Int, var name: String, var quantity: Int, var price: Double)

    var products = Array(
        Product(1, "Pallets", 50, 3500.00),
        Product(2, "Racks", 20, 25000.00),
        Product(3, "Forklifts", 20, 2500000.00),
        Product(4, "Pallet Jacks", 30, 75000.00),
        Product(5, "Conveyor Belts", 10, 150000.00),
        Product(6, "Storage Bins", 20, 1500.00),
        Product(7, "Label Printers", 20, 45000.00)
    )

    val inventory1 = Array(products(0).id, products(0).id, products(6).id)
    val inventory2 = Array(products(3).id, products(4).id, products(5).id)

    def printProductNames(inventory: Array[Int]): Unit = {
        val productNames = inventory.flatMap(id => products.find(_.id == id).map(_.name)).toSet
        println(productNames.mkString(", "))
    }

    def totalCost(inventory: Array[Int]): Double ={
        inventory.flatMap(id => products.find(_.id == id).map(_.price)).sum
    }

    def merge(inventory1: Array[Int], inventory2: Array[Int]) = {
        val mergedInventory = inventory1 ++ inventory2
        val productNames = mergedInventory.flatMap(id => products.find(_.id == id).map(_.name)).toSet
        println(s"Product names in merged inventory: ${productNames.mkString(", ")}")
        
        val totalQuantity = mergedInventory.length
        println(s"Total number of items of merged inventory: $totalQuantity")

        val totalCost = mergedInventory.flatMap(id => products.find(_.id == id).map(_.price)).sum
        println(s"Total cost of merged inventory: $totalCost")
    }

    def searchWithId(id: Int): Unit = {
    products.find(_.id == id) match {
        case Some(product) => println(s"Product found: $product")
        case None => println("Product Id does not exist")
    }
}

    println("Product names in inventory1: ")
    printProductNames(inventory1)

    println("Total cost of inventory1: ")
    println(totalCost(inventory1).formatted("%.2f"))

    println("Inventory1 is empty: " + inventory1.isEmpty)

    merge(inventory1, inventory2)

    searchWithId(1)

    // if (products.exists(_.id == 102)) {
    //     println(products.find(_.id == 102).get)
    // }
    // else {
    //     println("Product with ID 102 not found.")
    // }

    
    // printProductNames(mergedInventory)

    
}

