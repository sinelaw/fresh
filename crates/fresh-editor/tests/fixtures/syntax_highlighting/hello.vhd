-- VHDL example: a simple counter
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity hello_world is
    generic (
        WIDTH : integer := 8
    );
    port (
        clk      : in  std_logic;
        rst_n    : in  std_logic;
        data_in  : in  std_logic_vector(WIDTH-1 downto 0);
        data_out : out std_logic_vector(WIDTH-1 downto 0)
    );
end entity hello_world;

architecture rtl of hello_world is
    signal counter : unsigned(WIDTH-1 downto 0) := (others => '0');
begin

    process (clk, rst_n)
    begin
        if rst_n = '0' then
            counter  <= (others => '0');
            data_out <= (others => '0');
        elsif rising_edge(clk) then
            counter  <= counter + 1;
            data_out <= std_logic_vector(unsigned(data_in) xor counter);
        end if;
    end process;

    assert WIDTH > 0
        report "WIDTH must be positive"
        severity failure;

end architecture rtl;
