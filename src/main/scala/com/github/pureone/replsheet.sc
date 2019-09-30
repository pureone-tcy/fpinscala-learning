object Solution {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {

    val arr1: Array[Double] = nums1.map(_.toDouble)
    val arr2: Array[Double] = nums2.map(_.toDouble)

    if (arr1.length == 0)
      (arr2(0) + arr2(arr2.length - 1))/ 2
    else if (arr2.length == 0)
      (arr1(0) + arr1(arr1.length - 1))/ 2
    else if ((arr1.length + arr2.length) % 2 == 0)
      (arr1(0) + arr2(arr2.length-1)) / 2
    else
      (arr1(0) + arr2(0) + 1) / 2
  }
}

Solution.findMedianSortedArrays(Array(), Array(3,4))