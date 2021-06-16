import TestGraph from './TestGraph';
import { fireEvent } from "@testing-library/react";
import {ZOOM_INCREMENT, KEYBOARD_PANNING_INCREMENT} from '../Graph';

describe("Graph Navigation", () => {
	it("Should pan right when the right arrow key is pressed", async () => {
		await TestGraph.build();
		let svg = document.querySelector("svg");
		let initialX = parseInt(svg.getAttribute("viewBox").split(' ')[0]);
		fireEvent.keyDown(document.body, {key: 'ArrowRight', code: "ArrowRight"});
		let newX = parseInt(svg.getAttribute("viewBox").split(' ')[0]);
		let expected = initialX - KEYBOARD_PANNING_INCREMENT;
    expect(newX).toBe(expected);
	});

	it("Should pan left when the left arrow key is pressed", async () => {
		await TestGraph.build();
		let svg = document.querySelector("svg");
		let initialX = parseInt(svg.getAttribute("viewBox").split(' ')[0]);
		fireEvent.keyDown(document.body, {key: 'ArrowLeft', code: "ArrowLeft"});
		let newX = parseInt(svg.getAttribute("viewBox").split(' ')[0]);
		let expected = initialX + KEYBOARD_PANNING_INCREMENT;
    expect(newX).toBe(expected);
	});

	it("Should pan down when the down arrow key is pressed", async () => {
		await TestGraph.build();
		let svg = document.querySelector("svg");
		let initialY = parseInt(svg.getAttribute("viewBox").split(' ')[1]);
		fireEvent.keyDown(document.body, {key: 'ArrowDown', code: "ArrowDown"});
		let newY = parseInt(svg.getAttribute("viewBox").split(' ')[1]);
		let expected = initialY - KEYBOARD_PANNING_INCREMENT;
    expect(newY).toBe(expected);
	});

	it("Should pan up when the up arrow key is pressed", async () => {
		await TestGraph.build();
		let svg = document.querySelector("svg");
		let initialY = parseInt(svg.getAttribute("viewBox").split(' ')[1]);
		fireEvent.keyDown(document.body, {key: 'ArrowUp', code: "ArrowUp"});
		let newY = parseInt(svg.getAttribute("viewBox").split(' ')[1]);
		let expected = initialY + KEYBOARD_PANNING_INCREMENT;
    expect(newY).toBe(expected);
	});

	it("Should zoom in when the user presses the + key", async () => {
    await TestGraph.build();
    let svg = document.querySelector("svg");
    let initialDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    fireEvent.keyDown(document.body, {key: '+'});
    let newDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    let expectedDims = initialDims.map(dim => dim * (1 - ZOOM_INCREMENT));

    expect(newDims[0]).toBe(expectedDims[0]);
    expect(newDims[1]).toBe(expectedDims[1]);
	});

	it("Should zoom out when the user presses the - key", async () => {
    await TestGraph.build();
    let svg = document.querySelector("svg");
    let initialDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    fireEvent.keyDown(document.body, {key: '-'});
    let newDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    let expectedDims = initialDims.map(dim => dim * (1 + ZOOM_INCREMENT));

    expect(newDims[0]).toBe(expectedDims[0]);
    expect(newDims[1]).toBe(expectedDims[1]);
	});

	it("Should pan when the user clicks and drags", async () => {
    await TestGraph.build();
    let svg = document.querySelector("svg");
		let initialX = parseInt(svg.getAttribute("viewBox").split(' ')[0]);
		let initialY = parseInt(svg.getAttribute("viewBox").split(' ')[1]);
    fireEvent.mouseDown(svg, {clientX: 250, clientY: 10});
    fireEvent.mouseMove(svg, {clientX: 100, clientY:20});
    fireEvent.mouseUp(svg, {clientX:100, clientY:20});
		let newX = parseInt(svg.getAttribute("viewBox").split(' ')[0]);
		let newY = parseInt(svg.getAttribute("viewBox").split(' ')[1]);
    let expectedX = initialX + 150;
    let expectedY = initialY - 10;

    expect(newX).toBe(expectedX);
    expect(newY).toBe(expectedY);
	});

	it("Should zoom in when the mouse wheel is scrolled down", async () => {
    await TestGraph.build();
    let svg = document.querySelector("svg");
    let initialDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    let reactGraph = document.getElementById('react-graph');
    fireEvent.wheel(reactGraph, {deltaY: -1});
    let newDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    let expectedDims = initialDims.map(dim => dim * (1 - ZOOM_INCREMENT));
    expect(newDims[0]).toBe(expectedDims[0]);
    expect(newDims[1]).toBe(expectedDims[1]);
	});

	it("Should zoom out when the mouse wheel is scrolled up", async () => {
    await TestGraph.build();
    let svg = document.querySelector("svg");
    let initialDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    let reactGraph = document.getElementById('react-graph');
    fireEvent.wheel(reactGraph, {deltaY: 1});
    let newDims = svg.getAttribute("viewBox").split(' ').splice(2).map(dim => parseFloat(dim));
    let expectedDims = initialDims.map(dim => dim * (1 + ZOOM_INCREMENT));
    expect(newDims[0]).toBe(expectedDims[0]);
    expect(newDims[1]).toBe(expectedDims[1]);
	});
});