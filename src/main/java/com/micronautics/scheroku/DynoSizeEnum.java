package com.micronautics.scheroku;

public enum DynoSizeEnum {
    X1("1X"), X2("2X"), PX("PX");

    private final String size;

    DynoSizeEnum(String size) {
        this.size = size;
    }

    public String size() { return size; }
}
