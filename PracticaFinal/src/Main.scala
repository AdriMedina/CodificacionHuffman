/**
  * Created by Adri Medina on 09/06/2016.
  *
  * Clase para probar la funcionalidad
  *
  */
object Main extends App{

  val texto = "Hola hunndo"

  val tuplas = Huffman.obtenerTuplasOcurrencias(texto)
  println(tuplas)

  val orden = Huffman.generarListHojasOrdenadas(tuplas)
  println(orden)

  val arbol = Huffman.hasta(Huffman.singleton, Huffman.combinar)(orden)
  println(arbol)

}
