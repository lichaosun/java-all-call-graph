package test.call_graph.type;

/**
 * @author adrninistrator
 * @date 2022/10/24
 * @description:
 */
public class TestType1 {
    public void test1() {
        int i1 = '1';

        char c1 = '2';
        int i2 = c1;

        String s1 = "abc";
        int i3 = s1.charAt(0);

        System.out.println(i1 + " " + i2 + " " + i3);
    }

    public void test2() {
        char c1 = '1';
        if (c1 > 2) {
            System.getProperty("");
        }

        String s1 = "abc";
        char c2 = s1.charAt(0);
        if (c2 > 2) {
            System.setProperty("a", "b");
        }
    }
}
