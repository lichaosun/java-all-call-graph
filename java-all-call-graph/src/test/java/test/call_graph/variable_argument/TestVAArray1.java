package test.call_graph.variable_argument;

/**
 * @author adrninistrator
 * @date 2022/5/8
 * @description:
 */
public class TestVAArray1 {
    private void test1() {
        int[] array1 = new int[]{};
        int[] array2 = new int[]{1};
        System.out.println(array1[0]);
    }

    private void test2() {
        boolean[][] array1 = new boolean[][]{};
        boolean[][] array2 = new boolean[][]{new boolean[]{false}, new boolean[]{true, false}};

        boolean[] array1a = array1[0];
        System.out.println(array1a.length);
        System.out.println(array1a[0]);
    }
}
